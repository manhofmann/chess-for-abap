# Chess for ABAP

Chess engine and visualization in programming language ABAP

ABAP Version: 750 or higher

# Goals

- 100% ABAP Code
- Object oriented code
- Use Drag&Drop functionality to move pieces
- Multiplayer

# How does it look

Singleplayer (Report `ZCHESS_GAME_SOLOPLAYER`)

![singleplayer](https://github.com/user-attachments/assets/dfdfa14b-ae6e-4abb-980f-82f7796e1fcc)

Multiplayer (Report `ZCHESS_GAME_MULTIPLAYER`)

![multiplayer](https://github.com/user-attachments/assets/0bd3860e-3f0b-4d7c-bae8-46d600095718)

# How to play multiplayer

To play against a fellow ABAP developer you first need to start report `ZCHESS_GAME_SERVER`. It handles connection of new players (e.g. who plays white or black randomly). After game server has started each player may start report `ZCHESS_GAME_MULTIPLAYER`. 

# Installation

First you need to install [abapGit](https://docs.abapgit.org/user-guide/getting-started/install.html). Start report `ZABAPGIT_STANDALONE` and create a new repository with button _New Online_ and enter as _Git Repository URL_ the url of this repository:

![grafik](https://github.com/user-attachments/assets/e9c03794-269b-4732-9d41-18ea64c41558)

Click _Create Package_ and then _Create Online Repo_. Click _Pull_ and follow instructions.

# TODOs

- Implement a better bot/engine
- Move validation logic in multiplayer completely into server

# Credits and References

- Images of chess pieces were taken from [Green Chess](https://greenchess.net/info.php?item=downloads) and modified to insert white and gray background
- OO design from [RamziAl1/Chess](https://github.com/RamziAl1/Chess)
