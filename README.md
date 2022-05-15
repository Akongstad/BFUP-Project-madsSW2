# Scrabble bot: mads

## Steps Completed:
- Multi-player and dictionary (yes)
- Parsing boards playing on all boards (no)
- Parallelism (yes)
- Respect the timeout flag (yes, should respect the timeout flag, but will only pass or change pieces upon reaching it)


## Installation
https://dotnet.microsoft.com/en-us/download


## Usage

1. From project root 

cd to root folder

```bash
dotnet run --project ScrabbleTemplate
```
2. From ScrabbleTemplate project

cd to root folder
```bash
cd ScrabbleTemplate
dotnet run
```
3. Use JetBrains Rider deafult run configuration

If you wish to change the match configuration, eg make the bot play against itself
Go to ScrabbleTemplate/program.cs uncomment the lines setting the players variables
and uncomment or create the desired players collection.

## License
[MIT](https://choosealicense.com/licenses/mit/)
