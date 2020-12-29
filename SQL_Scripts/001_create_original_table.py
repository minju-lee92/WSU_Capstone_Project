import os
import sqlite3

DEFAULT_PATH = os.path.join(os.path.dirname(__file__), '..', 'Data', 'db.sqlite3')
DATA_PATH = os.path.join(os.path.dirname(__file__), '..', 'Data', 'WSU Capstone Project - SEL Distributed 02172020.xlsx')

def db_connect(db_path=DEFAULT_PATH):
    con = sqlite3.connect(db_path)
    print("Connected to " + db_path)
    return con

con = db_connect()

### Create original tables according to constraints
con.execute('''CREATE TABLE Customer_Info(
    [Customer.ID] INTEGER,
    [Overall.Customer.ID] INTEGER,
    [Customer.Size] INTEGER,
    PRIMARY KEY([Customer.ID])
);''')

con.execute('''CREATE TABLE Table_A(
    RowID INTEGER,
    CustomerLocation INTEGER,
    [Date] DATE,
    [Type] INTEGER,
    [Year] INTEGER,
    [Month] INTEGER,
    [Day] INTEGER,
    PRIMARY KEY(RowID),
    CONSTRAINT CustomerLocation FOREIGN KEY (CustomerLocation) REFERENCES Customer_Info([Customer.ID])
);''')

con.execute('''CREATE TABLE Table_B(
    RowID INTEGER,
    [Customer.Key] INTEGER,
    [Date] DATE,
    [Requested] INTEGER,
    [Early] INTEGER,
    [Late] INTEGER,
    [On.Time] INTEGER,
    [Year] INTEGER,
    [Month] INTEGER,
    [Day] INTEGER,
    PRIMARY KEY(RowID),
    CONSTRAINT fk_customer_id FOREIGN KEY ([Customer.Key]) REFERENCES Customer_Info([Customer.ID])
);''')

con.execute('''CREATE TABLE Table_D(
    RowID INTEGER,
    Customer INTEGER,
    [Date] DATE,
    [Type] INTEGER,
    [Year] INTEGER,
    [Month] INTEGER,
    [Day] INTEGER,
    PRIMARY KEY(RowID),
    CONSTRAINT fk_customer_id FOREIGN KEY (Customer) REFERENCES Customer_Info([Customer.ID])
);''')

con.execute('''CREATE TABLE Table_E(
    RowID INTEGER,
    Company INTEGER,
    Open DATE,
    [Open_Year] INTEGER,
    [Open_Month] INTEGER,
    [Open_Day] INTEGER,
    Closed DATE,
    [Closed_Year] INTEGER,
    [Closed_Month] INTEGER,
    [Closed_Day] INTEGER,
    PRIMARY KEY(RowID),
    CONSTRAINT fk_customer_id FOREIGN KEY (Company) REFERENCES Customer_Info([Company.ID])
);''')

print(con.execute('''SELECT * FROM Customer_Info'''))
