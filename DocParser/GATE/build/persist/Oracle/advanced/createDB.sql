REM
REM  SQLPlus script for Oracle 8.x
REM
REM  Copyright (c) 1998-2001, The University of Sheffield.
REM
REM  This file is part of GATE (see http://gate.ac.uk/), and is free
REM  software, licenced under the GNU Library General Public License,
REM  Version 2, June 1991 (in the distribution as file licence.html,
REM  and also available at http://gate.ac.uk/gate/licence.html).
REM
REM  Marin Dimitrov, 19/Sep/2001
REM
REM  $Id: createDB.sql 3460 2002-02-28 16:23:59Z marin $
REM
REM


spool install.log
whenever sqlerror continue

set termout     off
start createTable.sql
set termout     on
prompt >>>>>> Tables successfully  created...
clear buffer

set termout     off
@createSequence.sql
set termout     on
prompt  >>>>>> Sequences successfully  created...
clear buffer

set termout     off
@createIndex.sql
set termout     on
prompt >>>>>> Indexes successfully  created...
clear buffer

set termout     off
@alterIndex.sql
set termout     on
prompt >>>>>> Indexes successfully altered...
clear buffer

set termout     off
@createView.sql
set termout     on
prompt >>>>>> Views successfully  created...
clear buffer

set termout     off
@createType.sql
set termout     on
prompt >>>>>> Types successfully  created...
clear buffer

set termout     off
@../packages/error.spc
set termout     on
prompt >>>>>> Package ERROR successfully  created...
clear buffer

set termout     off
@../packages/security.spc
@../packages/security.bdy
set termout     on
prompt >>>>>> Package SECURITY successfully  created...
clear buffer

set termout     off
@../packages/persist.spc 
@../packages/persist.bdy
set termout     on
prompt >>>>>> Package PERSIST successfully  created...
clear buffer

set termout     off
@createTriggers.sql 
set termout     on
prompt >>>>>> Triggers successfully  created...
clear buffer

set termout     off
@initData.sql 
set termout     on
prompt >>>>>> Lookup tables successfully  initialized...
clear buffer

set termout     off
@grants.sql 
set termout     on
prompt >>>>>> Access to GATEADMIN objects successfully  granted to GATEUSER...
clear buffer

spool off

PROMPT
PROMPT DONE!
PROMPT

exit
