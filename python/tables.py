import os
import cx_Oracle
import sys

directory     = sys.argv[1]
db_connection = sys.argv[2]

if not os.path.exists(directory):
    os.makedirs(directory)

con = cx_Oracle.connect(db_connection)
cur = con.cursor()
cur.execute('select user, table_name from user_tables order by table_name')
for rec in cur:
    print (rec[0]+'.'+rec[1])
    os.system('sqlplus -s ' + db_connection + ' @../sql/table_creation.sql ' + rec[0] + ' ' + rec[1] + ' ' + directory)
cur.close()
con.close()