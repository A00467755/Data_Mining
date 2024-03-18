import pandas as pd
import sqlite3

con = sqlite3.connect("Chinook_Sqlite.sqlite")

cur = con.cursor()


# Artist
query = "select * from artist"

res = cur.execute(query)

artist = []

for result in res:
        #print(result)
        artist.append(result)
        
artist_df = pd.DataFrame(artist, columns=["id","artist_name"],)
        
# Album        
query2 = "select * from album"

res2 = cur.execute(query2)

album = [i for i in res2]

album_df = pd.DataFrame(album, columns=["id","album_title","album_id"],)


# Join two tables
album_df = album_df.join(artist_df, on=["album_id"],lsuffix="_l")

