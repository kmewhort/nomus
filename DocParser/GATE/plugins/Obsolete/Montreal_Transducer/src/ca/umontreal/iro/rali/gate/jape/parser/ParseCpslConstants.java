/* Generated By:JavaCC: Do not edit this line. ParseCpslConstants.java */
package ca.umontreal.iro.rali.gate.jape.parser;

public interface ParseCpslConstants {

  int EOF = 0;
  int space = 1;
  int spaces = 2;
  int newline = 3;
  int digits = 4;
  int letter = 5;
  int letters = 6;
  int lettersAndDigits = 7;
  int letterOrDigitOrDash = 8;
  int lettersAndDigitsAndDashes = 9;
  int multiphase = 10;
  int phases = 11;
  int path = 12;
  int phasesWhiteSpace = 13;
  int phasesSingleLineCStyleComment = 14;
  int phasesSingleLineCpslStyleComment = 15;
  int phasesCommentStart = 16;
  int phasesCommentChars = 17;
  int phasesCommentEnd = 18;
  int phase = 19;
  int input = 20;
  int option = 21;
  int rule = 22;
  int macro = 23;
  int priority = 24;
  int pling = 25;
  int kleeneOp = 26;
  int attrOp = 27;
  int integer = 28;
  int string = 35;
  int bool = 36;
  int ident = 37;
  int floatingPoint = 38;
  int exponent = 39;
  int colon = 40;
  int semicolon = 41;
  int period = 42;
  int bar = 43;
  int comma = 44;
  int leftBrace = 45;
  int rightBrace = 46;
  int leftBracket = 47;
  int rightBracket = 48;
  int assign = 49;
  int colonplus = 50;
  int whiteSpace = 51;
  int singleLineCStyleComment = 52;
  int singleLineCpslStyleComment = 53;
  int commentStart = 54;
  int commentChars = 55;
  int commentEnd = 56;
  int other = 57;

  int DEFAULT = 0;
  int IN_PHASES = 1;
  int PHASES_WITHIN_COMMENT = 2;
  int IN_STRING = 3;
  int WITHIN_COMMENT = 4;

  String[] tokenImage = {
    "<EOF>",
    "<space>",
    "<spaces>",
    "<newline>",
    "<digits>",
    "<letter>",
    "<letters>",
    "<lettersAndDigits>",
    "<letterOrDigitOrDash>",
    "<lettersAndDigitsAndDashes>",
    "\"Multiphase:\"",
    "\"Phases:\"",
    "<path>",
    "<phasesWhiteSpace>",
    "<phasesSingleLineCStyleComment>",
    "<phasesSingleLineCpslStyleComment>",
    "<phasesCommentStart>",
    "<phasesCommentChars>",
    "<phasesCommentEnd>",
    "\"Phase:\"",
    "\"Input:\"",
    "\"Options:\"",
    "\"Rule:\"",
    "\"Macro:\"",
    "\"Priority:\"",
    "\"!\"",
    "<kleeneOp>",
    "<attrOp>",
    "<integer>",
    "\"\\\"\"",
    "\"\\\\n\"",
    "\"\\\\r\"",
    "\"\\\\t\"",
    "\"\\\\\\\"\"",
    "<token of kind 34>",
    "\"\\\"\"",
    "<bool>",
    "<ident>",
    "<floatingPoint>",
    "<exponent>",
    "\":\"",
    "\";\"",
    "\".\"",
    "\"|\"",
    "\",\"",
    "\"{\"",
    "\"}\"",
    "\"(\"",
    "\")\"",
    "\"=\"",
    "\":+\"",
    "<whiteSpace>",
    "<singleLineCStyleComment>",
    "<singleLineCpslStyleComment>",
    "<commentStart>",
    "<commentChars>",
    "<commentEnd>",
    "<other>",
    "\"-->\"",
  };

}
