import pandas as pd

data = pd.read_csv("test.csv")

players = pd.DataFrame(columns=["Player name", "Player ID"])
player_games = pd.DataFrame(columns=["Player ID", "GameID"])
num_players = 0
next_id = 0
for index, row in data.iterrows():
    white = row["White"]
    black = row["Black"]
    player_ID_white = -1
    player_ID_black = -1
    #Check if players are in the players list
    for index, roww in players.iterrows():
        if(roww["Player name"] == white):
            player_ID_white = roww["Player ID"]
        if(roww["Player name"] == black):
            player_ID_black = roww["Player ID"]
        if(player_ID_black != -1 and player_ID_white!=-1):
            break
    
    #Add players to the Players list if it's not in it
    if(player_ID_black == -1):
        player_ID_black = next_id
        next_id += 1
        new_row = pd.DataFrame({"Player ID": [player_ID_black], "Player name": [black]})
        players = pd.concat([players, new_row], ignore_index=True)
    if(player_ID_white == -1):
        player_ID_white = next_id
        next_id += 1
        new_row = pd.DataFrame({"Player ID": [player_ID_white], "Player name": [white]})
        players = pd.concat([players, new_row], ignore_index=True)
    new_row = pd.DataFrame({"Player ID": [player_ID_white], "GameID": [row["GameID"]]})
    player_games = pd.concat([player_games, new_row], ignore_index=True)
    new_row = pd.DataFrame({"Player ID": [player_ID_black], "GameID": [row["GameID"]]})
    player_games = pd.concat([player_games, new_row], ignore_index=True)

player_games.to_csv('player_games.csv', index=False, header=True)
players.to_csv('players.csv', index=False, header=True)