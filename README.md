# runepal

A small CLI project to play with Elder Futhark runes.

Built to learn Haskell on practice.

## Setup

### Build

Use stack to build it:

```bash
stack build
```

Use stack to run:

```bash
stack run
```

### Install

Install `runepal` into your local binary space. For instance:

```bash
stack install --local-bin-path ~/.local/bin 
```

Add to `$PATH`, if needed:

```bash
export PATH=$HOME/.local/bin:$PATH
```

And run like this:

```bash
runepal-exe Thurisaz
```

Feel free to create a short alias:

```bash
alias rnp="runepal-exe"
```

## Usage

`runepal` prints random rune with some info by default:

```bash
rnp
```
```console
  ᛒ (b)
 Berkanan: birch
 Aett: Tyr
 Divination:
  Berkanan, literally birch, is a rune of female energy and 
  fertility. It could tell you that beauty will be faced and
  passion will be experienced. Berkanan encourages to enjoy 
  love, focus maternity and uncover tenderness.
```

You can print info for a rune of your choice:

```bash
rnp othala
```
```console
  ᛟ (o)
 Othala: heritage, estate, possession
 Aett: Tyr
 Divination:
  Othala stands for home, heritage -- possessions that are   
  not merely materialistic. The rune strongly reflects       
  ancestry and invaluable entities like family, kin, culture.
  The main message of Othala is to maintain your house and   
  cherish the tradition. 
```

Also, you can list the whole Elder Futhark and output rune data in CSV format.

```bash
rnp help
```
```console
Usage: command [arg]
  * no arg -- print data for random Futhark rune
  * rune name (case insensitive) -- print data for a rune of your choice
  * list -- list all Futhark runes
  * csv -- print all rune data in CSV format (pipe to file if needed)
  * help -- print this help message
```

