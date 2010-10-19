/*
 *  triggers.trg
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 18/Sep/2001
 *
 *  $Id: createTriggers.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */


create or replace trigger t_biu_lang_resource
  before insert or update on t_lang_resource  
  for each row  
declare
  -- local variables here
begin
  
  if (false = security.is_valid_security_data(:new.LR_ACCESS_MODE,
                                              :new.LR_OWNER_GROUP_ID,
                                              :new.LR_OWNER_USER_ID)) then

     raise error.x_incomplete_data;
     
  end if;
end t_bi_lang_resource;
/
