# 🃏 Blackjack

A team-built OCaml terminal blackjack game with chip betting, splitting, doubling down, and probability hints.

[Browse the source](https://github.com/jackyhu501-ctrl/Blackjack) · [Installation guide](INSTALL.md)

## Project purpose

Explore functional game-state modeling, card and hand logic, terminal interaction, and probability analysis in a playable team project.

## Access

Run locally in a terminal on macOS, Linux, or Windows using WSL as described below. No browser-playable version is linked in this repository.

## Tech stack

- OCaml for game logic and probability analysis
- Dune 3.22 or newer for builds
- ANSITerminal for colored output
- OUnit2 for tests
- opam for compiler and package management

## First-time setup

Install [Git](https://git-scm.com/downloads) and follow the official [OCaml/opam installation guide](https://ocaml.org/docs/installing-ocaml).

- **macOS:** use Terminal; install the command-line developer tools if requested by opam.
- **Linux:** use your terminal and install the build prerequisites requested by opam.
- **Windows:** these commands use [WSL with Ubuntu](https://learn.microsoft.com/en-us/windows/wsl/setup/environment). Install Git and opam inside WSL, then run the commands in the Ubuntu terminal. Native Windows opam is also available, but that path is not covered or tested here.

If opam is newly installed, initialize it once with `opam init`. Then:

```sh
git clone https://github.com/jackyhu501-ctrl/Blackjack.git
cd Blackjack
opam switch create . ocaml-base-compiler.5.3.0
eval $(opam env)
opam install 'dune>=3.22' ANSITerminal ounit2
dune build
dune exec bin/main.exe
```

If you already have a suitable OCaml switch, you can use it instead of creating a local switch. Install the dependencies in that switch.

## How to play

Follow the terminal prompts to place a chip bet and choose actions:

| Input | Action |
| --- | --- |
| `h` | Hit |
| `s` | Stand |
| `d` | Double down when offered |
| `p` | Split when offered |
| `t` | Toggle probability hints |
| `q` | Quit at the betting prompt |

Aces can count as 1 or 11. The game uses an eight-deck shoe (416 cards).

## Run again, test, and clean

```sh
cd Blackjack
eval $(opam env)
dune exec bin/main.exe
```

Run `dune test` for the test suite or `dune clean` to remove build artifacts.

## Troubleshooting

- **Missing ANSITerminal or OUnit2:** run `eval $(opam env)` and install the packages in the active switch.
- **Unsupported Dune language version:** install Dune 3.22 or newer.
- **Unreadable colors:** try another terminal color scheme; the game uses ANSI colors.

## Team and contributions

| Contributor | Role recorded in the original README |
| --- | --- |
| Alan Wu (ayw34) | Rendering and main program |
| Andrew Jang (sj627) | Game tests |
| Amy Xu (ax57) | Probability analysis |
| Jacky Hu (bh596) | Game logic |

## Verification

The existing project built successfully and passed all 14 tests on macOS with Dune 3.22.2. Fresh dependency installation and Windows/Linux execution have not been tested.
