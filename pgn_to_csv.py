import chess.pgn
import pandas as pd


filename = "test.csv"
columnNames = pd.DataFrame([["GameID","Event", "White", "Black", "Result", "Date", "Time", "WhiteElo", "BlackElo", "ECO", "Opening", "TimeControl", "Termination","Half Move Count"]])
columnNames.to_csv('test.csv', mode='w', index=False, header=False)

game_ID = 0
pgn = open("chess-data-mining/pgns/lichess_2013.pgn", encoding="utf-8")
while True:
    game = chess.pgn.read_game(pgn)
    if game == None:
        break
    headers = game.headers
    moveCount = (game.end().ply())
    row = pd.DataFrame([[game_ID, headers["Event"], headers["White"], headers["Black"],  headers["Result"],  headers["UTCDate"],  headers["UTCTime"], 
        headers["WhiteElo"], headers["BlackElo"], headers["ECO"], headers["Opening"], headers["TimeControl"], headers["Termination"], moveCount]])
    row.to_csv('test.csv', mode='a', index=False, header=False)
    game_ID += 1


print(headers)


