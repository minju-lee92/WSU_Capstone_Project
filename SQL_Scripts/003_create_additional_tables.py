import os
import sqlite3

DEFAULT_PATH = os.path.join(os.path.dirname(__file__), '..', 'Data', 'db.sqlite3')

def db_connect(db_path=DEFAULT_PATH):
    con = sqlite3.connect(db_path)
    print("Connected to " + db_path)
    return con

con = db_connect()

con.execute('''
DROP TABLE IF EXISTS `Table_A_Norm`;
''')

con.execute('''
DROP TABLE IF EXISTS `Table_B_Norm`;
''')

con.execute('''
DROP TABLE IF EXISTS `Table_D_Norm`;
''')

con.execute('''
DROP TABLE IF EXISTS `Table_E_Norm`;
''')

### Create aggregated tables
con.execute('''
CREATE TABLE Table_A_Norm(
    CustomerLocation INTEGER,
    [Date] DATE,
    [Type] INTEGER,
    [Year] INTEGER,
    [Month] INTEGER,
    [Day] INTEGER,
    Occurrences INTEGER,
    PRIMARY KEY(CustomerLocation, [Date], [Type]),
    CONSTRAINT CustomerLocation FOREIGN KEY (CustomerLocation) REFERENCES Customer_Info([Customer.ID])
);''')

con.execute('''
CREATE TABLE Table_B_Norm(
    [Customer.Key] INTEGER,
    [Date] DATE,
    [Requested] INTEGER,
    [Early] INTEGER,
    [Late] INTEGER,
    [On.Time] INTEGER,
    [Year] INTEGER,
    [Month] INTEGER,
    [Day] INTEGER,
    Occurrences INTEGER,
    PRIMARY KEY([Customer.Key], [Date], [Requested], [Early], [Late], [On.Time]),
    CONSTRAINT fk_customer_id FOREIGN KEY ([Customer.Key]) REFERENCES Customer_Info([Customer.ID])
);''')

con.execute('''
CREATE TABLE Table_D_Norm(
    Customer INTEGER,
    [Date] DATE,
    [Type] INTEGER,
    [Year] INTEGER,
    [Month] INTEGER,
    [Day] INTEGER,
    Occurrences INTEGER,
    PRIMARY KEY(Customer, [Date], [Type], [Year], [Month], [Day]),
    CONSTRAINT fk_customer_id FOREIGN KEY (Customer) REFERENCES Customer_Info([Customer.ID])
);''')

con.execute('''
CREATE TABLE Table_E_Norm(
    Company INTEGER,
    Open DATE,
    [Open_Year] INTEGER,
    [Open_Month] INTEGER,
    [Open_Day] INTEGER,
    Closed DATE,
    [Closed_Year] INTEGER,
    [Closed_Month] INTEGER,
    [Closed_Day] INTEGER,
    Occurrences INTEGER,
    PRIMARY KEY(Company, Open, Closed),
    CONSTRAINT fk_customer_id FOREIGN KEY (Company) REFERENCES Customer_Info([Company.ID])
);''')
