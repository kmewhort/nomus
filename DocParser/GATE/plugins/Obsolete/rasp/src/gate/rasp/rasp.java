package gate.rasp;

import java.io.*;
import java.util.*;
import java.net.URL;
import java.lang.*;

import gate.*;
import gate.creole.*;
import gate.util.*;
import gate.util.GateRuntimeException;


/**
 * This class is a wrapper for the rasp.
 */
 
 
public class rasp extends AbstractLanguageAnalyser
  implements ProcessingResource {
  
  //the document to be processed
  private gate.Document document;
  
  //the annotation set to be used for the generated annotations
  private String annotationSetName;
  
  // the rasp shell script
  private URL raspscript;
  
  private int textindx;
  private int wordID;
  private ArrayList wlist = new ArrayList(100);

  public Resource init()
    throws ResourceInstantiationException {return this;}

  public void execute() throws ExecutionException {
    //the rasp is only for Linux
    String osName = System.getProperty("os.name").toLowerCase();
    if (osName.indexOf("linux") == -1)
       throw new GateRuntimeException ("The RASP parser cannot be run on any other operating systems except Linux.");
    
    if(document == null)
      throw new GateRuntimeException("No document to process!");
    
    if(raspscript == null)
      throw new GateRuntimeException("Cannot proceed unless the runrasp script is specified.");
    textindx = 0; wordID=1;  
    //generate rasp command line
    String[] raspCmd = {"/bin/sh", "-c", raspscript.getFile()+" -m < "+document.getSourceUrl().getFile()};

    //run rasp
    runRASP(raspCmd);
    
  }


  private void runRASP(String[] cmdline) {
    String line;
    AnnotationSet aSet;

       
    if (annotationSetName !="") aSet = document.getAnnotations(annotationSetName);
       else aSet = document.getAnnotations();
    
    // run RASP and save output
    try {           
      Process p = Runtime.getRuntime().exec(cmdline);
              
      //get its RASP output (gate input)
      InputStream instream = p.getInputStream();
      BufferedReader input = new BufferedReader	(new InputStreamReader(instream));
       
      while ((line = input.readLine())!= null) {
        if (line.trim().length()==0) continue;//System.out.println(line);
  	if (line.startsWith("(\"") ) {
 	   if (wlist.size()>0) { setrasptoken(aSet);
 	   } 
 	   wlist.clear(); 	   
           tokenline(line); 	    	   
 	}
 	else { if (line.startsWith("(|"))       // line starts with "(|"
                 {relationline(line, aSet);}
               else {
                  // throw new GateRuntimeException("unexpected output of rasp.");
                     continue;       //ignor the line
               }	  
 	} 
 	 
      }  // end while - finish reading lines
      if (wlist.size()>0) { setrasptoken(aSet);
      }
      wlist.clear();
    } 
    catch (Exception err) {
      System.out.println( "RASP Exception: " + err );
    }
       
  } //end runRASP

  public void setrasptoken(AnnotationSet annset){
         AnnotationSet aSet = annset;
         for (Iterator iter = wlist.iterator(); iter.hasNext();){
             Word aword = (Word) iter.next();
             if (aword.id !=0){
             FeatureMap map = Factory.newFeatureMap();
             map.put("POS", aword.POS);
             map.put("Morph", aword.Morph);
             map.put("string", aword.content);
             map.put("length", Integer.toString(aword.content.length()));
             try{
                aSet.add(/*aword.id,*/ new Long(aword.startoffset), new Long(aword.endoffset), "RASPToken", map);
             } catch (InvalidOffsetException ioe){
               throw new GateRuntimeException("Invalid offset of the annotation");
             }}
         }     
  }
  
  public void tokenline(String ln){
           
           String sContent = document.getContent().toString();           
           String line = ln.trim();
   	   int pre=line.indexOf("\"");
 	   int last = line.lastIndexOf("\"");
 	   for (int i=pre;pre!=-1 && pre<last;){
 	       int next = line.indexOf("\"", pre+1);
 	       String st=line.substring(pre+1, next); 
 	       pre = line.indexOf("\"",next+1);
 	       Word wd= new Word();
 	       wd.content=st;
 	       wd.startoffset = sContent.indexOf(st, textindx);
 	       wd.endoffset = wd.startoffset + st.length();
               if (wd.startoffset !=0)
               { String tmpstr = sContent.substring(0,wd.startoffset).trim();
                 if (tmpstr.endsWith("\""))
                    { Word empwd = new Word();
                      empwd.id=0;
                      wlist.add(empwd);
                      wordID = wordID+1;
                    }
               }
 	       //wd.id = wordID;
 	       wordID = wordID+1;
 	       textindx = wd.endoffset;
 	       wlist.add(wd);
 	   }
  }
  
  public void relationline(String ln, AnnotationSet annset){
           AnnotationSet aSet = annset;
           String line = ln.trim();
   	   int index = line.indexOf("|");
 	   int next = line.indexOf("|", index+1);
  	   String st = line.substring(index+1, next).trim();
 	   if ((st.equals("ncsubj"))||(st.equals("csubj"))||(st.equals("xsubj"))||(st.equals("obj"))||
 	       (st.equals("dobj"))||(st.equals("subj"))){
               head_dep_op(st, line.substring(next+1), aSet);
 	   }
 	   else {
 	     if ((st.equals("xcomp"))||(st.equals("aux"))||(st.equals("iobj"))||(st.equals("dependent"))||(st.equals("mod"))||
 	          (st.equals("cmod"))||(st.equals("xmod"))||(st.equals("ncmod"))||(st.equals("detmod"))||(st.equals("ccomp")))
 	     {
 	        op_head_dep(st, line.substring(next+1), aSet);
 	     }
 	     else {
 	       if (st.equals("conj")) {
 	        op_heads(st, line.substring(next+1), aSet);
 	       }
 	       else {
 	         if (st.equals ("arg_mod")) {
 	            op_head_dep_op(st, line.substring(next+1), aSet);
 	         }
 	         else {
 	           if ((st.equals("arg"))||(st.equals("subj_or_dobj"))||(st.equals("comp"))||
 	               (st.equals("obj2"))||(st.equals("clausal"))) {
 	              head_dep(st, line.substring(next+1), aSet);
 	           }
 	           else {
 	                 throw new GateRuntimeException ("unrecognized relationship");
 	           }
 	         }
 	       }
 	     }
 	   } 	   
 	       
  }
  
  public void head_dep_op(String key,String ln, AnnotationSet annset){
          AnnotationSet aSet = annset;
          String kword=key;
          String line = ln.trim();
          String op, head, dep;
          int begin, next, startoffset, endoffset, start, end, wID;
          String span;
 
          begin=startoffset = endoffset =0;
          
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          head = proc_span(span);
          start=end=Integer.valueOf(head).intValue();
          
          line = line.substring(next+1).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          dep = proc_span(span);
          wID = Integer.valueOf(dep).intValue();
          if (start > wID) start = wID;
          if (end < wID) end = wID;
          
          line = line.substring(next+1).trim();
          next = line.indexOf("|", begin+1);
          if ((next!=-1)&&(next > begin)) {
            span = line.substring(begin+1, next);
            if (span.endsWith("\\")){
               next = line.indexOf("|", next+1);
               span = line.substring(begin+1, next);
            }
            op = proc_span(span);
            wID = Integer.valueOf(op).intValue();
            if (wID==0) {op=null;}
            else {if (start > wID) start = wID;
                  if (end < wID) end = wID;
            }
          }
          else {
            //op = ""; 
              op = null;
          }
          
          FeatureMap map = Factory.newFeatureMap();
          map.put("other parameter", op);
          map.put("head", head);
          map.put("dependent", dep);
          Word wd = (Word)wlist.get(start-1);
          startoffset = wd.startoffset;
          Word wd2 = (Word)wlist.get(end-1);
          endoffset = wd2.endoffset;
          
          try{
             aSet.add(new Long(startoffset), new Long(endoffset), kword, map);
          } catch (InvalidOffsetException ioe){
              throw new GateRuntimeException("Invalid offset of the annotation");
          }
  }
  
  public String proc_span(String st){
          String span = st.trim();
          String key;
          Word wd;
          int comindx, dashindx, wID;

          comindx = span.lastIndexOf(":");          
          if (comindx ==-1)
          { key = "0"; return key;}                         
          dashindx = span.lastIndexOf("_");
          key = span.substring(comindx+1, dashindx);
          wID = Integer.valueOf(key).intValue();
          wd= (Word)wlist.get(wID-1);
          wd.id = wID;
          wd.Morph = span.substring(0, comindx);  
          wd.POS = span.substring(dashindx+1);
          return key;  
  }
  
  public void op_head_dep(String key, String ln, AnnotationSet annset){
          AnnotationSet aSet = annset;
          String kword=key;
          String line = ln.trim();
          String op, head, dep;
          int begin, next, startoffset, endoffset, start, end, wID;
          String span;
                
          begin=next=startoffset = endoffset =start = end=0;

          if (line.startsWith("_")){
             op = null;
             next = line.indexOf("|");            
          }
          else {         // starts with "|"
                next = line.indexOf("|", begin+1);
                span = line.substring(begin+1, next);
                if (span.endsWith("\\")){
                  next = line.indexOf("|", next+1);
                  span = line.substring(begin+1, next);
                }
                op = proc_span(span);
                start=end=Integer.valueOf(op).intValue();
                if (start ==0) op=null;
                next=next+1;
          }
          
          line = line.substring(next).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          head = proc_span(span);
          wID = Integer.valueOf(head).intValue();
          if (op==null) {start = end = wID;}
          else {
                 if (start>wID) start = wID;
                 if (end <wID) end = wID;
               }
          
          line = line.substring(next+1).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          dep = proc_span(span);
          wID = Integer.valueOf(dep).intValue();
          if (start > wID) start = wID;
          if (end < wID) end = wID;
           
          FeatureMap map = Factory.newFeatureMap();
          map.put("other parameter", op);
          map.put("head", head);
          map.put("dependent", dep);
          Word wd = (Word)wlist.get(start-1);
          startoffset = wd.startoffset;
          Word wd2 = (Word)wlist.get(end-1);
          endoffset = wd2.endoffset; 
          
          try{
             aSet.add(new Long(startoffset), new Long(endoffset), kword, map);
          } catch (InvalidOffsetException ioe){
              throw new GateRuntimeException("Invalid offset of the annotation");
          }          
          
  }

  public void head_dep (String key, String ln, AnnotationSet annset){
          AnnotationSet aSet = annset;
          String kword=key;
          String line = ln.trim();
          String op, head, dep;
          int begin, next, startoffset, endoffset, start, end, wID;
          String span;
 
          begin=startoffset = endoffset =0;
          
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          head = proc_span(span);
          start=end=Integer.valueOf(head).intValue();
          
          line = line.substring(next+1).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          dep = proc_span(span);
          wID = Integer.valueOf(dep).intValue();
          if (start > wID) start = wID;
          if (end < wID) end = wID;          
       
          FeatureMap map = Factory.newFeatureMap();
          map.put("head", head);
          map.put("dependent", dep);
          Word wd = (Word)wlist.get(start-1);
          startoffset = wd.startoffset;
          Word wd2 = (Word)wlist.get(end-1);
          endoffset = wd2.endoffset;
          
          try{
             aSet.add(new Long(startoffset), new Long(endoffset), kword, map);
          } catch (InvalidOffsetException ioe){
              throw new GateRuntimeException("Invalid offset of the annotation");
          }          
  }
  
  public void op_head_dep_op(String key, String ln, AnnotationSet annset){
          AnnotationSet aSet = annset;
          String kword=key;
          String line = ln.trim();
          String[] op=new String[2];
          String head, dep;
          int begin, next, startoffset, endoffset, start, end, wID;
          String span;
                
          begin=next=startoffset = endoffset =start = end=0;

          if (line.startsWith("_")){
             op[0] = null; 
             next = line.indexOf("|");            
          }
          else {         // starts with "|"
                next = line.indexOf("|", begin+1);
                span = line.substring(begin+1, next);
                if (span.endsWith("\\")){
                  next = line.indexOf("|", next+1);
                  span = line.substring(begin+1, next);
                }
                op[0] = proc_span(span);
                start=end=Integer.valueOf(op[0]).intValue();
                if (start ==0) op[0]=null;
                next = next+1;
          }
          
          line = line.substring(next).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          head = proc_span(span);
          wID = Integer.valueOf(head).intValue();
          if (op[0]==null) {start = end = wID;}
          else {
                 if (start > wID) start = wID;
                 if (end < wID) end = wID;
               }
          
          line = line.substring(next+1).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          dep = proc_span(span);
          wID = Integer.valueOf(dep).intValue();
          if (start > wID) start = wID;
          if (end < wID) end = wID;
          
          line = line.substring(next+1).trim();
          next = line.indexOf("|", begin+1);
          if (next ==-1) {
               op[1]=null;
          }
          else {
               span = line.substring(begin+1, next);
               op[1]=proc_span(span);
               wID = Integer.valueOf(op[1]).intValue();
               if (wID==0) {op[1]=null;}
               else {if (start > wID) start = wID;
                     if (end< wID) end = wID;
               }
          }
          
          FeatureMap map = Factory.newFeatureMap();
          map.put("other parameter", op);
          map.put("head", head);
          map.put("dependent", dep);
          Word wd = (Word)wlist.get(start-1);
          startoffset = wd.startoffset;
          Word wd2 = (Word)wlist.get(end-1);
          endoffset = wd2.endoffset; 
          
          try{
             aSet.add(new Long(startoffset), new Long(endoffset), kword, map);
          } catch (InvalidOffsetException ioe){
              throw new GateRuntimeException("Invalid offset of the annotation");
          }                      
  }

  public void op_heads(String key, String ln, AnnotationSet annset){
          AnnotationSet aSet = annset;
          String kword=key;
          String line = ln.trim();
          String op, dep;
          String[] head = new String[2];
          int begin, next, startoffset, endoffset, start, end, wID;
          String span;
                
          begin=next=startoffset = endoffset =start = end=0;

          if (line.startsWith("_")){
             op = null; 
             next = line.indexOf("|");            
          }
          else {         // starts with "|"
                next = line.indexOf("|", begin+1);
                span = line.substring(begin+1, next);
                if (span.endsWith("\\")){
                   next = line.indexOf("|", next+1);
                   span = line.substring(begin+1, next);
                }
                op = proc_span(span);
                start=end=Integer.valueOf(op).intValue();
                if (start ==0) op=null;                
                next = next+1;
          }
          
          line = line.substring(next).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          head[0] = proc_span(span);
          wID = Integer.valueOf(head[0]).intValue();
          if (op==null) {start = end = wID;}
          else {
                 if (start > wID) start = wID;
                 if (end < wID) end = wID;
               }
          
          line = line.substring(next+1).trim();
          next = line.indexOf("|", begin+1);
          span = line.substring(begin+1, next);
          if (span.endsWith("\\")){
             next = line.indexOf("|", next+1);
             span = line.substring(begin+1, next);
          }
          head[1] = proc_span(span);
          wID = Integer.valueOf(head[1]).intValue();
          if (start > wID) start = wID;
          if (end < wID) end = wID;

           
          FeatureMap map = Factory.newFeatureMap();
          map.put("other parameter", op);
          map.put("head", head);
          Word wd = (Word)wlist.get(start-1);
          startoffset = wd.startoffset;
          Word wd2 = (Word)wlist.get(end-1);
          endoffset = wd2.endoffset; 
          
          try{
             aSet.add(new Long(startoffset), new Long(endoffset), kword, map);
          } catch (InvalidOffsetException ioe){
              throw new GateRuntimeException("Invalid offset of the annotation");
          }                      
  }

  // getter and setter methods
  
  //set the document
  public void setDocument(gate.Document document){
       this.document = document;
  }
  
  //return the document
  public gate.Document getDocument(){
       return document;
  }
 
  //set the annotation set name
  public void setAnnotationSetName(String annotationSetName){
       this.annotationSetName = annotationSetName;
  }
  
  //return the annotation set name
  public String getAnnotationSetName(){
       return annotationSetName;
  }
  
  //set the raspscript value
  public void setRaspscript(URL raspscript) {
    this.raspscript = raspscript;
  }
 
  //return the raspscript value 
  public URL getRaspscript() {
    return raspscript;
  }
  
} // class RASP

class Word {
  public int id;
  public int startoffset;
  public int endoffset;
  public String content;
  public String POS;
  public String Morph;
  
}
