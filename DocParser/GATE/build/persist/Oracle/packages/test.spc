/*
 *  test.spc
 *
 *  Copyright (c) 1998-2001, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 25/Sep/2001
 *
 *  $Id: test.spc 3462 2002-02-28 16:26:47Z marin $
 *
 */

create or replace package test is
    
                      
  procedure create_test_data;

  procedure remove_test_data;  
  
  procedure remove_all_data;    

end test;
/
