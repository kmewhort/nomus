/*
 *  PrioritisedRuleList.java - transducer class
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Hamish Cunningham, 27/07/98
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file to the ca.umontreal.iro.rali.gate.jape package.
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import java.util.*;


/**
  * A list of rules ordered according to priority. May be used for ordering
  * non-matched rules (in which case the order is based on
  * priority/position), or matched rules (in which case the order is based
  * on matched lenght/priority/position). Note that position represents
  * the idea of order within an input file; it is assumed that this is the
  * same as the order of addition of the rules to the list, i.e. a rule
  * added 5th is assumed to occupy 5th place in the file (or other rule
  * source). This class is based on JGL's DList, which allows for fast
  * insertion of elements at any point. The highest priority rule is the
  * first in the list, which may be accessed by <CODE>front()</CODE>.
  */
public class PrioritisedRuleList extends ArrayList implements java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Adds a rule in order. Used for non-matched rules. Implements the
    * ordering based on priority/position.
    */
  public synchronized void add(Rule newRule) {
    /* for each rule,
     *   if it is higher priority, continue;
     *   else if it is same priority
     *     if it is higher position, continue;
     *     else break
     *   else (it is lower priority) break
     * insert newRule before current position (which may be finish)
     */
    Iterator iterator = this.iterator();
    int i = 0;
    for(  ; iterator.hasNext(); i++) {
      Rule rule	=	(Rule) iterator.next();
      int rulePriority =	rule.getPriority();
      int newRulePriority =	newRule.getPriority();
      int rulePosition =	rule.getPosition();
      int newRulePosition =	newRule.getPosition();

      if(rulePriority > newRulePriority)
        continue;
      else if(rulePriority == newRulePriority) {
        if(rulePosition < newRulePosition)
          continue;
        else
          break;
      } else {
        break;
      }

    } // while not hit the end of the rules


    this.add(i, newRule);
  } // add(Rule)

  /** Adds a rule in order. Used for matched rules. Implements the
    * ordering based on length/priority/position. Length is given as
    * a parameter.
    */
  public synchronized void add(Rule newRule, int newRuleLength) {
    /* for each rule,
     *   if it is longer than the new one, continue;
     *   else if it is the same length
     *     if it is higher priority, continue;
     *     else if it is same priority
     *       if it is higher position, continue;
     *       else break;
     *     else (it is lower priority) break;
     *   else (it is shorter) break;
     * insert newRule before current position (which may be finish)
     */
    Iterator iterator = this.iterator();
    int i = 0;
    for(  ; iterator.hasNext(); i++) {
      Rule rule	=	(Rule) iterator.next();
      int rulePriority =	rule.getPriority();
      int newRulePriority =	newRule.getPriority();
      int rulePosition =	rule.getPosition();
      int newRulePosition =	newRule.getPosition();
      int ruleLength = rule.getEndPosition() - rule.getStartPosition();

      if(ruleLength > newRuleLength)
        continue;
      else if(ruleLength == newRuleLength) {
        if(rulePriority > newRulePriority)
          continue;
        else if(rulePriority == newRulePriority) {
          if(rulePosition < newRulePosition)
            continue;
          else
            break;
        } else {
          break;
        }
      } else {
        break;
      }

    } // while not hit the end of the rules

    add(i, newRule);
  } // add(Rule,int)

} // class PrioritisedRuleList

