#!/usr/bin/python
import sqlite3
conn = sqlite3.connect("pheno.db")  # connect to database
c = conn.cursor()  # make a cursor object
# Create tables
c.execute("""CREATE TABLE IF NOT EXISTS trainingData (EntryID INTEGER PRIMARY KEY,
          Pop TEXT, FruitID TEXT, Gid TEXT, Weight REAL, Length REAL,
          Width REAL, Brix REAL, DM REAL, L REAL,
          a REAL, b REAL) """)
c.execute("""CREATE TABLE IF NOT EXISTS gainTrials (EntryID TEXT PRIMARY KEY, Pop TEXT,
          Year INTEGER, Site TEXT, Block INTEGER, Plot INTEGER,
          Gid TEXT, Fruit TEXT,  Weight REAL, Length REAL, Width REAL,
          Brix REAL, DM REAL, L REAL, a REAL, b REAL)""")
c.execute("""CREATE TABLE IF NOT EXISTS trainingYield (EntryID INTEGER PRIMARY KEY,
            Pop TEXT, Gid TEXT UNIQUE, TotalFrtCt REAL, TotalWtKg REAL)""")
# Populate traingData
# data = ["./C0/ALL/C0_final.csv", "./C2/ALL/C2_final.csv", "./T1/ALL/T1_final.csv"]
# pops = ["C0","C2", "T1"]
# idx = 1
# for p, f in enumerate(data):
    # with open(f) as fh:
        # _ = fh.readline()  # throw away the header line
        # for  line in fh:
            # tmp = []
            # for entry in line.strip().split(","):
                # if entry == "NA":  # replace entry 'NA' with 'NULL' for SQL compliance
                    # tmp.append(None)
                # else:
                    # tmp.append(entry)
            # 
            # c.execute("""INSERT INTO trainingData VALUES (?,?,?,?,?,?,?,?,?,?,?,?)""",
            # (idx, pops[p], tmp[0], tmp[1], tmp[2], tmp[3], tmp[4], tmp[5], tmp[6], tmp[7],
             # tmp[8], tmp[9]))
            # idx += 1
# Populate Yield 
# data= ["./C0/Yield/C0_final_yield.csv","./C2/Yield/C2_yield.csv", "./T1/Yield/T1_yield.csv"]
# pops = ["C0","C2", "T1"]
# idx = 1
# for p, f in enumerate(data):
    # with open(f) as fh:
        # _ = fh.readline()  # throw away the header line
        # for line in fh:
            # tmp = []
            # for entry in line.strip().split(","):
                # if entry == "NA":  # replace entry 'NA' with 'NULL' for SQL compliance
                    # tmp.append(None)
                # else:
                    # tmp.append(entry)
            # c.execute("""INSERT INTO trainingYield VALUES (?,?,?,?,?)""",
            # (idx, pops[p], tmp[0], tmp[1], tmp[2]))
            # idx += 1
# Populate gainTrials
ID_to_pop={"18-3723":"C0", "18-3724":"C1", "18-3725":"C4", 
           "18-3726":"Honeynut", "18-3727":"Bugle", "18-3728":"Amber",
           "17-180":"C0", "17-181":"C1", "17-182":"C4", "17-187":"C2",
           "17-183":"Honeynut", "17-184":"Bugle", "17-185":"Amber"}
data = ["./Gain_Trial_2018/All/Gain2018_final.csv", "./Gain_Trial_2017/ALL/Gain2017_final.csv"]
for f in data:
    with open(f) as fh:
        _ = fh.readline() 
        for line in fh:
            tmp = []
            for entry in line.strip().split(","):
                if entry == "NA":
                    tmp.append(None)
                else:
                    tmp.append(entry)
            try:
                c.execute("""INSERT INTO gainTrials VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)""",
                         (tmp[0], ID_to_pop[tmp[1]], tmp[2], tmp[3], tmp[4], tmp[5], tmp[6],
                          tmp[7], tmp[8], tmp[9], tmp[10], tmp[11], tmp[12], tmp[13], tmp[14],
                          tmp[15]))
            except KeyError:
                print("Possible type:", tmp[1])

conn.commit()  # commit all changes
conn.close()  # close database connection
