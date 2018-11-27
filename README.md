# haskell53 [![GitHub license](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg?style=flat)](http://www.apache.org/licenses/LICENSE-2.0)

Special assignment for the functional programming course: A DNS server made in Haskell

## Getting started

These instructions will install the system in your local machine.

### Prerequsites

1. Clone the repository

    ```
    $ git clone https://github.com/juanmbellini/haskell53.git
    ```

2. Install Stack if you haven't yet

    ```
    $ brew install stack
    ```

    **Check [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/) for more information on how to install Stack.**


### Build

To build the project, execute the following commands.

```
$ cd haskell53 # Or project root's directory
$ stack setup
$ stack build
```

**Note:** The ```stack setup``` command installs an isolated copy of GHC for the project.

### Run

To run the project, execute the following command (after building, previous section)

```
$ stack exec haskell53-exe
```

## Author

- [Juan Marcos Bellini](https://github.com/juanmbellini)
