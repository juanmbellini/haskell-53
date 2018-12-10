module Dns
    ( T.Domain
    , T.ResourceType(..)
    , T.ResourceClass(..)
    , T.ResourceTTL
    , T.ResourceData(..)
    , T.MailExchanger(..)
    , T.ResourceRecord(..)
    , T.Zone(..)
    , T.DnsCacheSystem(..)
    , S.DnsServerConfig(..)
    , S.startDnsServer
    ) where

import Dns.Types as T
import Dns.DnsServer as S
