<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

# Table of Contents

- [Plutarch Trie](#plutarch-trie)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
    - [Trie](#trie)
    - [Plutarch Trie](#plutarch-trie-implementation)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
    - [How to use](#how-to-use)

<!-- markdown-toc end -->

# Plutarch Trie

## Introduction

The Plutarch Trie project provides a Plutarch-based implementation of Distributed Tries for the Cardano blockchain. This project allows developers to leverage the security and efficiency of Tries in their Cardano smart contracts, ensuring data integrity and efficient data verification. This project uniquely allows scalable data structures across multiple utxos, with a developer-friendly typescript api.

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-osde-open-source-dev-ecosystem/anastasia-labs-the-trifecta-of-data-structures-tries-tries-and-linked-lists-for-cutting-edge-contracts) and is aimed at enhancing the capabilities of Cardano smart contracts in handling complex data structures.

## Documentation

### Trie

A trie, also known as a prefix tree or digital tree, is a kind of search tree—an ordered tree data structure used to store a dynamic set or associative array where the keys are usually strings. Unlike a binary search tree, no node in the tree stores the key associated with that node; instead, its position in the tree defines the key with which it is associated. This makes tries extremely useful for applications like autocomplete systems, spell checkers, and IP routing. Here's a detailed explanation:

#### Basic concept

A Trie is a type of search tree, an ordered tree data structure that is used to store a dynamic set or associative array where the keys are usually strings. Here's how it's structured:

- **Leaf Nodes**: These are the nodes at the bottom of the tree that contain values associated with the keys. The keys are formed by the path from the root to the leaf.
- **Non-Leaf (Intermediate) Nodes**: These nodes contain the common prefixes of the keys or parts of them. They help in reducing the search space for a query.
- **Root Node**: The single node at the top of the tree represents the starting point of every key stored in the trie. It is usually empty.

#### Key Encoding

The core of a Trie is its key encoding mechanism. This mechanism takes input keys of any size and encodes them into a path through the Trie. Each character or byte of the key represents a step down the Trie, from the root towards the leaves.

#### Construction

- **Inserting Keys**: Keys are inserted by walking through the Trie according to the encoded path of the key. If a path does not exist, new nodes are created to accommodate it.
- **Node Structure**: Each node in the Trie can have several children, each representing a possible continuation of the key. The value associated with a key is stored in the leaf node at the end of its path.
- **Prefix Sharing**: Nodes share common prefixes, which makes Tries highly space-efficient for datasets with keys that share common prefixes.

#### Features

- **Efficiency in Search Operations**: Tries allow for efficient search operations, including lookups, insertions, and deletions, all with time complexity proportional to the length of the key.
- **Prefix Matching**: Tries excel at prefix matching, allowing for quick searches of all keys that share a common prefix, which is useful for autocomplete systems and spell checkers.
- **Space Efficiency**: By sharing common prefixes among keys, Tries use space more efficiently than other data structures like hash tables, especially when the dataset contains many similar keys.

#### Example

Consider a Trie with the keys "car", "cat", and "dog":
The structure of the Trie after inserting the keys "car", "cat", and "dog" would look something like this:

Imagine the Trie as a tree where each node represents a character. The root node is empty and branches out to three paths: one for "c", one for "d", and potentially others for different starting letters of keys not shown in this example. The "c" node branches into "a", which further branches into "r" and "t" to form the words "car" and "cat". Each of these nodes, "r" and "t", would be leaf nodes for "car" and "cat", respectively, possibly containing values or simply marking the end of the word. Similarly, the "d" node branches into "o", which then branches into "g", forming the word "dog" with "g" as its leaf node. This structure allows for efficient searching, adding, and deleting of keys by following the branches corresponding to each character in the key.

This visual representation helps understand how Tries optimize space and search time, especially with a large number of keys sharing common prefixes. By sharing the initial "ca" in "car" and "cat", the Trie saves space compared to storing each word independently. This efficiency becomes more pronounced with a larger dataset with more shared prefixes.

### Plutarch Trie implementation

The Plutarch Trie implementation is a smart contract solution that allows the creation of many distributed trie's from a single validator.

## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org/download.html) installed on your system. Nix is used for package management and to provide a consistent development environment.
To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.18.1
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target you can use with the `nix develop` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

### Building and developing

You should be able to seamlessly use the repository to
develop, build and run plutarch-trie.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/plutarch-trie.git
```

Navigate to the repository directory:

```sh
cd plutarch-trie
direnv allow
```

Activate the development environment with Nix:

```sh
nix develop .
```

Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
cabal run build
```

Execute the test suite:

```sh
cabal test
```

![plutarch-trie.gif](/assets/images/plutarch-trie.gif)

### How to use

To integrate the Plutarch Trie library into your project, start by creating a `cabal.project` file, as this file is not automatically generated during the cabal init process. Then, proceed by including the following:

```filename="cabal.project"
source-repository-package
  type: git
  location: git://github.com/Anastasia-Labs/plutarch-trie.git
```

Then in the your `[name of your project].cabal` file include the repository under the build-depends section

```filename="[name of your project].cabal" {3}
build-depends:
    base ^>= 4.11.1.0
  , plutarch-trie
```
