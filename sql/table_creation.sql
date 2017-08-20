define ip_table_owner=&1
define ip_table_name=&2
define ip_path=&3

set serveroutput on
set linesize 32000
set trimspool on
set echo off
set feedback off
set timing off
set termout off
set pagesize 0
set heading off
set verify off
set arraysize 5000

spool '&ip_path\&ip_table_owner..&ip_table_name..sql'

DECLARE
  ip_table_name VARCHAR2(30) := '&ip_table_name';
  ip_table_owner     VARCHAR2(30) := '&ip_table_owner';
  l_script           CLOB;
  l_datatype         VARCHAR2(100);
  l_nullable         VARCHAR2(30);
  l_column           VARCHAR2(32000);
  l_data_default     VARCHAR2(4000);
  l_tablespace_name  VARCHAR2(30);
  l_logging          VARCHAR2(30);
  l_partitioned      VARCHAR2(30);
  l_monitoring       VARCHAR2(30);
  l_compression      VARCHAR2(30);
  l_cache            VARCHAR2(30);
  l_result_cache     VARCHAR2(30);
  l_table_comment    VARCHAR2(4000);
  l_search_condition VARCHAR2(4000);
  l_cnt_costr        NUMBER;
BEGIN
  l_script := 'SET DEFINE OFF;' || chr(10);
  l_script := l_script || 'Prompt Table ' || ip_table_name || ';' || chr(10);
  l_script := l_script || '--' || chr(10);
  l_script := l_script || '-- ' || ip_table_name || '  (Table) ' || chr(10);
  l_script := l_script || '--' || chr(10);
  l_script := l_script || 'CREATE TABLE ' || ip_table_owner || '.' ||
              ip_table_name || chr(10);
  l_script := l_script || '(' || chr(10);

  -----------------------------------------------------------------------
  -- Columns
  -----------------------------------------------------------------------
  FOR rec IN (SELECT table_name,
                     column_name,
                     data_type,
                     data_type_mod,
                     data_type_owner,
                     decode(data_type,
                            'CHAR',
                            char_length,
                            'VARCHAR',
                            char_length,
                            'VARCHAR2',
                            char_length,
                            'NCHAR',
                            char_length,
                            'NVARCHAR',
                            char_length,
                            'NVARCHAR2',
                            char_length,
                            NULL) char_data_length,
                     data_length,
                     data_precision,
                     data_scale,
                     nullable,
                     char_used,
                     virtual_column,
                     --USER owner,
                     MAX(length(column_name)) over(PARTITION BY NULL) AS max_col_name_length,
                     default_length,
                     data_default,
                     row_number() over(PARTITION BY NULL ORDER BY column_id DESC) last_column
                FROM user_tab_cols tc
               WHERE 1 = 1
                 AND hidden_column = 'NO'
                 AND table_name = ip_table_name
               ORDER BY table_name, column_id) LOOP
    --ToDo: long column names in 12c (128 chars)
    --ToDo: check all possible types
    l_column   := '  ' || rpad(rec.column_name, rec.max_col_name_length);
    l_datatype := rec.data_type || CASE
                    WHEN rec.char_data_length IS NOT NULL THEN
                     '(' || rec.char_data_length || CASE
                       WHEN rec.char_used = 'B' THEN
                        ' BYTE'
                       WHEN rec.char_used = 'C' THEN
                        ' CHAR'
                       ELSE
                        NULL
                     END || ')'
                    ELSE
                     NULL
                  END || CASE
                    WHEN rec.data_precision IS NOT NULL THEN
                     '(' || rec.data_precision || ',' || rec.data_scale || ')'
                    ELSE
                     NULL
                  END;
  
    l_nullable := NULL;
    FOR null_cond IN (SELECT c.search_condition
                        FROM user_constraints c, user_cons_columns cc
                       WHERE cc.owner = c.owner
                         AND cc.constraint_name = c.constraint_name
                         AND cc.table_name = c.table_name
                         AND c.constraint_type = 'C'
                         AND c.table_name = rec.table_name
                         AND cc.column_name = rec.column_name) LOOP
      l_search_condition := null_cond.search_condition;
      IF l_search_condition LIKE '%IS NOT NULL' THEN
        l_nullable := 'NOT NULL';
      END IF;
    END LOOP;
  
    l_data_default := rec.data_default;
  
    IF l_nullable IS NOT NULL OR l_data_default IS NOT NULL THEN
      l_column := rpad(l_column || '  ' || l_datatype, 48);
    ELSE
      l_column := l_column || '  ' || l_datatype;
    END IF;
  
    IF l_data_default IS NOT NULL THEN
      IF l_nullable IS NOT NULL THEN
        l_column := l_column ||
                    rpad('DEFAULT ' || l_data_default,
                         greatest(length(l_data_default) + 1, 30));
      ELSE
        l_column := l_column || 'DEFAULT ' || l_data_default;
      END IF;
    
    END IF;
  
    l_column := l_column || l_nullable;
  
    l_column := l_column || CASE
                  WHEN rec.last_column != 1 THEN
                   ','
                  ELSE
                   NULL
                END;
  
    l_script := l_script || l_column || chr(10);
  END LOOP;

  l_script := l_script || ')' || chr(10);

  -----------------------------------------------------------------------
  -- Table properties
  -----------------------------------------------------------------------
  SELECT tablespace_name,
         logging,
         partitioned,
         monitoring,
         compression,
         cache,
         result_cache
    INTO l_tablespace_name,
         l_logging,
         l_partitioned,
         l_monitoring,
         l_compression,
         l_cache,
         l_result_cache
    FROM (SELECT table_type_owner,
                 table_type,
                 object_id_type,
                 table_name,
                 tablespace_name,
                 buffer_pool,
                 iot_name,
                 iot_type,
                 min_extents,
                 next_extent,
                 max_extents,
                 ini_trans,
                 max_trans,
                 initial_extent,
                 pct_increase,
                 freelists,
                 freelist_groups,
                 pct_free,
                 pct_used,
                 instances,
                 cluster_name,
                 degree,
                 num_rows,
                 avg_row_len,
                 temporary,
                 logging,
                 partitioned,
                 NESTED,
                 row_movement,
                 monitoring,
                 duration,
                 dependencies,
                 compression,
                 compress_for,
                 'NO' read_only,
                 cache,
                 flash_cache,
                 cell_flash_cache,
                 'DEFAULT' result_cache,
                 segment_created
            FROM user_object_tables t
           WHERE 1 = 1
             AND ((iot_type IS NULL) OR (iot_type <> 'IOT_MAPPING'))
             AND ((table_name = ip_table_name) OR
                 (table_name LIKE 'SYS_IOT_OVER%' AND iot_name = ip_table_name))
          UNION ALL
          SELECT NULL,
                 NULL,
                 NULL,
                 table_name,
                 tablespace_name,
                 buffer_pool,
                 iot_name,
                 iot_type,
                 min_extents,
                 next_extent,
                 max_extents,
                 ini_trans,
                 max_trans,
                 initial_extent,
                 pct_increase,
                 freelists,
                 freelist_groups,
                 pct_free,
                 pct_used,
                 instances,
                 cluster_name,
                 degree,
                 num_rows,
                 avg_row_len,
                 temporary,
                 logging,
                 partitioned,
                 NESTED,
                 row_movement,
                 monitoring,
                 duration,
                 dependencies,
                 compression,
                 compress_for,
                 read_only,
                 cache,
                 flash_cache,
                 cell_flash_cache,
                 result_cache,
                 segment_created
            FROM user_tables t
           WHERE 1 = 1
             AND ((iot_type IS NULL) OR (iot_type <> 'IOT_MAPPING'))
             AND ((table_name = ip_table_name) OR
                 (table_name LIKE 'SYS_IOT_OVER%' AND iot_name = ip_table_name)));

  -----------------------------------------------------------------------
  -- lobs
  -----------------------------------------------------------------------
  FOR lobs IN (SELECT table_name,
                      segment_name,
                      column_name,
                      index_name,
                      chunk,
                      pctversion,
                      decode(cache, 'YES', 'CACHE', 'NOCACHE') cache,
                      decode(logging, 'YES', 'LOGGING', 'NOLOGGING') logging,
                      decode(in_row,
                             'YES',
                             'ENABLE       STORAGE IN ROW',
                             'DISABLE      STORAGE IN ROW') in_row,
                      freepools,
                      decode(retention, NULL, NULL, '  RETENTION' || chr(10)) retention,
                      tablespace_name,
                      encrypt,
                      compression,
                      deduplication,
                      decode(securefile, 'YES', 'SECUREFILE ', NULL) securefile
                 FROM sys.user_lobs
                WHERE 1 = 1
                  AND table_name = ip_table_name) LOOP
    l_script := l_script || 'LOB (' || lobs.column_name || ') STORE AS ' ||
                lobs.securefile || '(' || chr(10);
    l_script := l_script || '  TABLESPACE ' || lobs.tablespace_name || chr(10);
    l_script := l_script || '  ' || lobs.in_row || chr(10);
    l_script := l_script || '  CHUNK       ' || lobs.chunk || chr(10);
    l_script := l_script || lobs.retention;
    l_script := l_script || '  ' || lobs.cache || chr(10);
    l_script := l_script || '  ' || lobs.logging || chr(10);
  
    FOR lob_str IN (SELECT segment_name,
                           partition_name,
                           segment_type,
                           tablespace_name,
                           to_char(initial_extent / 1024) || 'K' initial_extent,
                           to_char(next_extent / 1024 / 1024) || 'M' next_extent,
                           buffer_pool,
                           min_extents,
                           max_extents,
                           pct_increase,
                           freelists,
                           freelist_groups,
                           flash_cache,
                           cell_flash_cache
                      FROM user_segments
                     WHERE 1 = 1
                       AND segment_type = 'LOBSEGMENT'
                       AND segment_name = lobs.segment_name) LOOP
      -- ToDo: MAXEXTENTS, PCTINCREASE
      l_script := l_script || '      STORAGE    (' || chr(10);
      l_script := l_script || '                  INITIAL          ' ||
                  lob_str.initial_extent || chr(10);
      l_script := l_script || '                  NEXT             ' ||
                  lob_str.next_extent || chr(10);
      l_script := l_script || '                  MINEXTENTS       ' ||
                  lob_str.min_extents || chr(10);
      l_script := l_script || '                  MAXEXTENTS       UNLIMITED' ||
                  chr(10);
      l_script := l_script || '                  PCTINCREASE      0' || chr(10);
      l_script := l_script || '                  BUFFER_POOL      ' ||
                  lob_str.buffer_pool || chr(10);
      l_script := l_script || '                  FLASH_CACHE      ' ||
                  lob_str.flash_cache || chr(10);
      l_script := l_script || '                  CELL_FLASH_CACHE ' ||
                  lob_str.cell_flash_cache || chr(10);
      l_script := l_script || '                 ))' || chr(10);
    END LOOP;
  
  END LOOP;

  -- ToDo: investigate all possible cases
  IF l_logging = 'YES' THEN
    l_logging := 'LOGGING';
  ELSE
    l_logging := 'NOLOGGING';
  END IF;

  IF l_compression = 'DISABLED' THEN
    l_compression := 'NOCOMPRESS';
  ELSE
    l_compression := '???';
  END IF;

  IF l_cache = '    N' THEN
    l_cache := 'NOCACHE';
  ELSE
    l_cache := '???';
  END IF;

  IF l_monitoring = 'YES' THEN
    l_monitoring := 'MONITORING';
  ELSE
    l_monitoring := '???';
  END IF;

  l_script := l_script || 'TABLESPACE ' || l_tablespace_name || chr(10);
  l_script := l_script || 'RESULT_CACHE (MODE ' || l_result_cache || ')' ||
              chr(10);
  l_script := l_script || l_logging || ' ' || chr(10);
  l_script := l_script || l_compression || ' ' || chr(10);
  l_script := l_script || l_cache || chr(10);
  l_script := l_script || 'NOPARALLEL' || chr(10); --???
  l_script := l_script || l_monitoring || ';' || chr(10);
  l_script := l_script || chr(10);

  -----------------------------------------------------------------------
  -- Comments
  -----------------------------------------------------------------------
  BEGIN
    SELECT c.comments
      INTO l_table_comment
      FROM user_tab_comments c
     WHERE c.table_name = ip_table_name
       AND c.comments IS NOT NULL;
    l_script := l_script || 'COMMENT ON TABLE ' || ip_table_owner || '.' ||
                ip_table_name || ' IS ''' || l_table_comment || ''';' ||
                chr(10) || chr(10);
  EXCEPTION
    WHEN no_data_found THEN
      NULL;
  END;

  FOR rec IN (SELECT c.column_name, c.comments
                FROM user_col_comments c, user_tab_columns tc
               WHERE c.table_name = tc.table_name
                 AND c.column_name = tc.column_name
                 AND c.comments IS NOT NULL
                 AND c.table_name = ip_table_name
               ORDER BY tc.column_id) LOOP
    l_script := l_script || 'COMMENT ON COLUMN ' || ip_table_owner || '.' ||
                ip_table_name || '.' || rec.column_name || ' IS ''' ||
                replace(rec.comments, '''', '''''') || ''';' || chr(10) || chr(10);
  END LOOP;

  l_script := l_script || chr(10) || chr(10);

  -----------------------------------------------------------------------
  -- Indexes
  -----------------------------------------------------------------------
  --ToDo: logging, compression, parallel, other index types
  FOR rec IN (SELECT index_name,
                     uniqueness,
                     decode(logging, 'YES', 'LOGGING', 'NOLOGGING') logging,
                     tablespace_name,
                     decode(degree, 1, 'NOPARALLEL', 'PARALLEL ' || degree) parallel_degree,
                     (SELECT '(' || listagg(c.column_name, ', ') within GROUP(ORDER BY c.column_position) || ')'
                        FROM user_ind_columns c
                       WHERE c.index_name = i.index_name
                         AND c.table_name = i.table_name
                       GROUP BY c.index_name) ind_columns,
                     table_owner,
                     table_name
                FROM user_indexes i
               WHERE i.index_type != 'LOB'
                 AND table_owner || '.' || table_name =
                     ip_table_owner || '.' || ip_table_name) LOOP
    l_script := l_script || 'Prompt Index ' || rec.index_name || ';' || chr(10);
    l_script := l_script || '--' || chr(10);
    l_script := l_script || '-- ' || rec.index_name || '  (Index) ' || chr(10);
    l_script := l_script || '--' || chr(10);
    l_script := l_script || '--  Dependencies: ' || chr(10);
    l_script := l_script || '--   ' || rec.table_name || ' (Table)' || chr(10);
    l_script := l_script || '--' || chr(10);
    l_script := l_script || 'CREATE UNIQUE INDEX ' || ip_table_owner || '.' ||
                rec.index_name || ' ON ' || rec.table_owner || '.' ||
                rec.table_name || chr(10);
    l_script := l_script || rec.ind_columns || chr(10);
    l_script := l_script || rec.logging || chr(10);
    l_script := l_script || 'TABLESPACE ' || rec.tablespace_name || chr(10);
    l_script := l_script || rec.parallel_degree || ';' || chr(10);
  END LOOP;

  l_script := l_script || chr(10);

  -----------------------------------------------------------------------
  -- Non FK Constraints
  -----------------------------------------------------------------------
  SELECT COUNT(*)
    INTO l_cnt_costr
    FROM user_constraints
   WHERE constraint_type != 'R'
     AND table_name = ip_table_name;

  IF l_cnt_costr != 0 THEN
    l_script := l_script || chr(10) || '-- ';
    l_script := l_script || chr(10) ||
                '-- Non Foreign Key Constraints for Table ' || ip_table_name || ' ';
    l_script := l_script || chr(10) || '-- ';
    l_script := l_script || chr(10) ||
                'Prompt Non-Foreign Key Constraints on Table ' || ip_table_name || ';';
    l_script := l_script || chr(10) || 'ALTER TABLE ' || ip_table_owner || '.' ||
                ip_table_name || ' ADD (';
  END IF;

  -----------------------------------------------------------------------
  -- Primary keys
  -----------------------------------------------------------------------
  --ToDo: check validate, status, etc.
  FOR rec IN (SELECT c.owner,
                     c.constraint_name,
                     CASE c.constraint_type
                       WHEN 'P' THEN
                        '  PRIMARY KEY' || chr(10) || '  ' ||
                        (SELECT '(' || listagg(cc.column_name, ', ') within GROUP(ORDER BY cc.position) || ')'
                           FROM user_cons_columns cc
                          WHERE cc.owner = c.owner
                            AND cc.constraint_name = c.constraint_name
                            AND cc.table_name = c.table_name
                          GROUP BY cc.constraint_name) || chr(10) ||
                        '  USING INDEX ' || c.index_owner || '.' || c.index_name
                       WHEN 'C' THEN
                        '  CHECK (<search_condition>)'
                       WHEN 'U' THEN
                        '  UNIQUE ' ||
                        (SELECT '(' || listagg(cc.column_name, ', ') within GROUP(ORDER BY cc.position) || ')'
                           FROM user_cons_columns cc
                          WHERE cc.owner = c.owner
                            AND cc.constraint_name = c.constraint_name
                            AND cc.table_name = c.table_name
                          GROUP BY cc.constraint_name) || chr(10) ||
                        '  USING INDEX ' || c.index_owner || '.' || c.index_name
                       ELSE
                        NULL
                     END clause,
                     decode(c.status, 'ENABLED', 'ENABLE') status,
                     decode(c.validated, 'VALIDATED', 'VALIDATE', 'NOVALIDATE') ||
                     decode(row_number()
                            over(PARTITION BY NULL ORDER BY c.constraint_type DESC,
                                 c.constraint_name DESC),
                            1,
                            ');',
                            ',') validated,
                     c.index_owner,
                     c.index_name,
                     (SELECT '(' || listagg(cc.column_name, ', ') within GROUP(ORDER BY cc.position) || ')'
                        FROM user_cons_columns cc
                       WHERE cc.owner = c.owner
                         AND cc.constraint_name = c.constraint_name
                         AND cc.table_name = c.table_name
                       GROUP BY cc.constraint_name) cons_columns,
                     c.search_condition,
                     c.constraint_type
                FROM user_constraints c
               WHERE c.constraint_type != 'R'
                 AND c.table_name = ip_table_name
               ORDER BY c.constraint_type, c.constraint_name) LOOP
    l_search_condition := rec.search_condition;
  
    IF rec.constraint_type = 'C' AND l_search_condition LIKE '%IS NOT NULL' THEN
      CONTINUE;
    END IF;
  
    l_script := l_script || chr(10) || '  CONSTRAINT ' || rec.constraint_name;
  
    IF rec.constraint_type = 'C' THEN
      l_script := l_script || chr(10) ||
                  REPLACE(rec.clause, '<search_condition>', l_search_condition);
    ELSE
      l_script := l_script || chr(10) || rec.clause;
    END IF;
  
    l_script := l_script || chr(10) || '  ' || rec.status || ' ' ||
                rec.validated;
  END LOOP;
  --ToDo: all other Non FK constraints

  -----------------------------------------------------------------------
  -- Grants
  -----------------------------------------------------------------------
  -- ToDo: grantor, grantable, HIERARCHY, OBJECT_TYPE
  FOR rec IN (SELECT t.grantee,
                     t.table_schema,
                     t.table_name,
                     t.grantor,
                     t.privilege,
                     t.grantable,
                     t.hierarchy,
                     o.object_type
                FROM sys.all_tab_privs t, sys.all_objects o
               WHERE t.table_schema = o.owner
                 AND t.table_name = o.object_name
                 AND o.object_type IN ('TABLE', 'INDEXTYPE', 'TYPE')
                 AND t.table_schema = ip_table_owner
                 AND table_name = ip_table_name
               ORDER BY 1, 2, 3, 5) LOOP
    l_script := l_script || chr(10) || chr(10) || 'Prompt Privs on TABLE ' ||
                rec.table_name || ' TO ' || rec.grantee || ' to ' ||
                rec.grantee || ';';
    l_script := l_script || chr(10) || 'GRANT ' || rec.privilege || ' ON ' ||
                rec.table_schema || '.' || rec.table_name || ' TO ' ||
                rec.grantee || ';';
  
  END LOOP;

  dbms_output.put_line(l_script);
END;
/
spool off
exit;
