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
REM  $Id: createDBDev.sql 3465 2002-02-28 16:39:31Z marin $
REM
REM

spool dev.log
whenever sqlerror continue

set termout     off
@../packages/test.spc 
@../packages/test.bdy
set termout     on
prompt >>>>>> Package TEST successfully  created...
clear buffer

set termout     off
exec test.create_test_data();
commit;
set termout     on
prompt >>>>>> test data successfully  inserted...
clear buffer

spool off

exit