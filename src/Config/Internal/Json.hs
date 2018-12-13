{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Config.Internal.Json 
    ( parseSystemConfig
    ) where


import Dns  ( Domain
            , ResourceType(..)
            , ResourceClass(..)
            , DnsServerConfig(..)
            , MailExchanger(..)
            , ResourceData(..)
            , ResourceRecord(..)
            , Zone(..)
            )
import Config.SystemConfig

import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.IP
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L (ByteString)


-- ================================================================================================
-- Parsing
-- ================================================================================================

-- | Parses a SystemConfig from the given Lazy ByteString.
parseSystemConfig :: L.ByteString -> Maybe SystemConfig
parseSystemConfig = fmap theSystemConfig . decode

-- ================================================================================================
-- Newtype declartions with needed class definitions to avoid orphan instances of FromJSON
-- ================================================================================================

-- | Wrapper for Dns.Types.Domain (which, in turn, is an alias for ByteString).
newtype OwnDomain = OwnDomain {theDomain :: Domain}

-- | Wrapper for ByteString, in order to be treated directly as text data.
newtype TxtData = TxtData {txt :: C.ByteString}

-- | Wrapper for IPv4.
newtype IPv4Wrapper = IPv4Wrapper {theIPv4 :: IPv4}

-- | Wrapper for IPv6.
newtype IPv6Wrapper = IPv6Wrapper {theIPv6 :: IPv6}

-- | Wrapper for MailExchanger.
newtype MailExchangerWrapper = MailExchangerWrapper {theExchanger :: MailExchanger}

-- | Wrapper for ResourceType.
newtype ResourceTypeWrapper = ResourceTypeWrapper {theType :: ResourceType}

-- | Wrapper for ResourceClass.
newtype ResourceClassWrapper = ResourceClassWrapper {theClass :: ResourceClass}

-- | Wrapper for ResourceData.
newtype ResourceDataWrapper = ResourceDataWrapper {theData :: ResourceData}

-- | Wrapper for ResourceRecord.
newtype ResourceRecordWrapper = ResourceRecordWrapper {theRecord :: ResourceRecord}

-- | Wrapper for Zone.
newtype ZoneWrapper = ZoneWrapper {theZone :: Zone}

-- | Wrapper for DnsServerConfig.
newtype DnsServerConfigWrapper = DnsServerConfigWrapper {theConfig :: DnsServerConfig}

-- | Wrapper for SystemConfig.
newtype SystemConfigWraper = SystemConfigWraper {theSystemConfig :: SystemConfig}

-- | A Typeclass to mark types that can act as a ByteString representation.
class ByteStringTransformer a where
    fromByteString  :: C.ByteString -> a

-- | An instance of ByteStringTransformer for OwnDomain.
instance ByteStringTransformer OwnDomain where
    fromByteString = OwnDomain

-- | An instance of ByteStringTransformer for TxtData.
instance ByteStringTransformer TxtData where
    fromByteString = TxtData


-- ================================================================================================
-- FromJSON instances
-- ================================================================================================

-- byteStringParser :: ByteStringTransformer a => (C.ByteString -> a) -> T.Text -> Parser a
byteStringParser :: ByteStringTransformer a => T.Text -> Parser a
byteStringParser = return . fromByteString . C.pack . T.unpack

-- | A FromJSON instance for OwnDomain (reading values as text)
instance FromJSON OwnDomain where
    -- parseJSON = withText "domain" $ byteStringParser OwnDomain
    parseJSON = withText "domain" $ byteStringParser

instance FromJSON TxtData where
    parseJSON = withText "txt" $ byteStringParser
    -- parseJSON = withText "domain" $ return . OwnDomain . C.pack . T.unpack

-- | A FromJSON instance for IPv4 (reading values as text)
instance FromJSON IPv4Wrapper where
    parseJSON = withText "address" $ return . IPv4Wrapper . read . T.unpack

-- | A FromJSON instance for IPv6 (reading values as text)
instance FromJSON IPv6Wrapper where
    parseJSON = withText "address" $ return . IPv6Wrapper . read . T.unpack

-- | A FromJSON instance for MailExchanger (reading values as an Object)
instance FromJSON MailExchangerWrapper where
    parseJSON = withObject "exchanger" $ \o -> do
        -- exchange    <- o .: "exchange" >>= return . toByteString
        exchange    <-  o .: "exchange" <$$> theDomain
        preference  <-  o .: "preference"
        return . MailExchangerWrapper $ MailExchanger{..}

-- | A FromJSON instance for ResourceTypeWrapper (reading values as text)
instance FromJSON ResourceTypeWrapper where
    parseJSON = withText "type" selection
        where
            returnWrapper :: ResourceType -> Parser ResourceTypeWrapper
            returnWrapper = return . ResourceTypeWrapper
            selection :: T.Text -> Parser ResourceTypeWrapper
            selection t = case t of
                "A"     -> returnWrapper A
                "NS"    -> returnWrapper NS
                "CNAME" -> returnWrapper CNAME
                "SOA"   -> returnWrapper SOA
                "MX"    -> returnWrapper MX
                "TXT"   -> returnWrapper TXT
                "AAAA"  -> returnWrapper AAAA
                "PTR"   -> returnWrapper PTR
                _       -> fail "Unsupported resource type"

-- | A FromJSON instance for ResourceClassWrapper (reading values as text)
instance FromJSON ResourceClassWrapper where
    parseJSON = withText "class" selection
        where
            selection :: T.Text -> Parser ResourceClassWrapper
            selection t = case t of
                "IN"    -> return . ResourceClassWrapper $ IN
                _       -> fail "Unsupported resource class"

-- | A FromJSON instance for ResourceDataWrapper (reading values as an Object)
instance FromJSON ResourceDataWrapper where
    parseJSON = withObject "data" $ \o -> asum . toWrappers $ [
                    A_Data      <$> o .: "addresses" <$$> map theIPv4,
                    NS_Data     <$> o .: "servers" <$$> map theDomain,
                    CNAME_Data  <$> o .: "cname" <$$> theDomain,
                    SOA_Data    <$> o .: "primaryServer" <$$> theDomain
                                            <*> o .: "email" <$$> theDomain
                                            <*> o .: "serial" 
                                            <*> o .: "refresh" 
                                            <*> o .: "retry" 
                                            <*> o .: "expire" 
                                            <*> o .: "minimumTtl",
                    MX_Data     <$> o .: "exchangers" <$$> map theExchanger,
                    TXT_Data    <$> o .: "txt" <$$> txt,
                    AAAA_Data   <$> o .: "addresses6" <$$> map theIPv6,
                    PTR_Data    <$> o .: "domains" <$$> map theDomain   
                ]
        where
            toWrappers :: [Parser ResourceData] -> [Parser ResourceDataWrapper]
            toWrappers = map $ fmap ResourceDataWrapper

-- | A FromJSON instance for ResourceRecordWrapper (reading values as an Object)
instance FromJSON ResourceRecordWrapper where
    parseJSON = withObject "record" $ \o -> do
        name            <- o .: "name" <$$> theDomain
        resourceType    <- o .: "type" <$$> theType
        resourceClass   <- o .: "class" <$$> theClass
        resourceTtl     <- o .: "ttl"
        resourceData    <- o .: "data" <$$> theData
        return . ResourceRecordWrapper $ ResourceRecord{..}

-- | A FromJSON instance for ZoneWrapper (reading values as an Object)
instance FromJSON ZoneWrapper where
    parseJSON = withObject "zone" $ \o -> do
        domain  <- o .: "domain" <$$> theDomain
        records <- o .: "recordSet" <$$> map theRecord
        return . ZoneWrapper $ Zone{..}

-- | A FromJSON instance for DnsServerConfigWrapper (reading values as an Object)
instance FromJSON DnsServerConfigWrapper where
    parseJSON = withObject "config" $ \o -> do
        listeningAddress    <- o .: "interface"
        listeningPort       <- o .: "port"
        nameServers         <- o .: "forwardTo"
        rootServers         <- o .: "roots" <$$> map theIPv4
        zones               <- o .: "zones" <$$> map theZone
        return . DnsServerConfigWrapper $ DnsServerConfig{..}

-- | A FromJSON instance for SystemConfig
--   (reading values as an Object without nesting, but all in the same JSON).
instance FromJSON SystemConfigWraper where
    parseJSON = withObject "config" $ \o -> do
        dnsConfig           <- parseJSON (Object o) <$$> theConfig
        cacheDataFilePath   <- o .: "cache"
        return . SystemConfigWraper $ SystemConfig{..}

-- ================================================================================================
-- Helpers
-- ================================================================================================

-- | Flipped version of (<$>), created for syntax sugar when declaring FromJSON instances.
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
