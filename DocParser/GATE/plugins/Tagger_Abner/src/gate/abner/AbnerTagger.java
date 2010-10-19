/*
 *	Wrapper over Abner: http://pages.cs.wisc.edu/~bsettles/abner/
 *  AbnerTagger.java
 *  Tue Jul 21 18:49:33 EEST 2009
 *  Georgi Georgiev, georgi.georgiev@ontotext.com
 */

package gate.abner;

import gate.Annotation;
import gate.AnnotationSet;
import gate.Factory;
import gate.FeatureMap;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import gate.util.InvalidOffsetException;

import java.io.IOException;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.apache.log4j.Logger;

import abner.Tagger;


/**
 * Abner wrapper for gate. This tagger is based on CRF statistical modeling
 * and is taken off the shelf. It also uses its own build in tokenizer and
 * sentence split. One can use without any other nlp components.
 * The tagger has two models:
 * 1. Trained on GENIA
 * 2. Trained on NLPBA
 * @author <A HREF="mailto:georgiev@ontotext.com>georgi.georgiev@ontotext.com</A>
 * Created: Thu Dec 11 16:25:59 EET 2008
 */

public @SuppressWarnings("all") class AbnerTagger extends AbstractLanguageAnalyser {
	public static final long serialVersionUID = 1L;

    /*private members*/
	private Boolean toBeTolkenized = true;
	private AbnerRunMode abnerMode = null;
	private String outputASName = null;
	private Long gateDocumentIndex = null;
	// abner tagger
	private Tagger abnerTagger;
	
	//add parameter to define the anno type defaut to Abner
	
	//enumeration for the modes
	
	//remove namespace part

	private static final Logger logger = Logger.getLogger(AbnerTagger.class);

	private Map<String, Integer> modes = new HashMap<String, Integer>();
	
	//private String abnerMode;

	/* getters and setters for the PR */
	/*public members*/
	public void setToBeTolkenized(Boolean a) {
		toBeTolkenized = a;
	}

	public Boolean getToBeTolkenized() {
		return toBeTolkenized;
	}

	public void setAbnerMode(AbnerRunMode a) {
		abnerMode = a;
	}

	public AbnerRunMode getAbnerMode() {
		return abnerMode;
	}

	public void setOutputASName(String a) {
		outputASName = a;
	}

	public String getOutputASName() {	
		return outputASName;
	}/* getters and setters for the PR */

	// may be needed
	// public String outputASName = null;

	public Resource init() throws ResourceInstantiationException {
		/* Should be added with a property file */
		modes.put("NLPBA", Tagger.NLPBA);
		modes.put("BIOCREATIVE", Tagger.BIOCREATIVE);
		logger.info("Abner intializing the "+abnerMode+" model...");
//		logger.info("in the map: "+modes.get(abnerMode.toString())+" model...");
//		logger.info("the map: "+modes.toString()+" model...");
		abnerTagger = new Tagger(modes.get(abnerMode.toString()));//modes.get(abnerMode));
		gateDocumentIndex = 0L;

		return this;
	}

	public void reInit() throws ResourceInstantiationException {
		init();
	}


	/**
	 * @author georgiev this is the method that calls abner Tagger.getSegments
	 *         methods annotations are returned as string chunks with attached
	 *         meta tags in vector of 2D arrays containing respectively 1. string
	 *         chunks in first array 2. corresponding meta tags in second array
	 *         one element of the vector contains one sentence in terms of abner
	 *         (JLex) tokenization.
	 */
	public void execute() throws ExecutionException {
		gateDocumentIndex = 0L;

		if (!(abnerTagger.getMode() == modes.get(getAbnerMode().toString()))) {
			try {
				if (modes.containsKey(getAbnerMode().toString()))
					abnerTagger = new Tagger(modes.get(getAbnerMode().toString()));
				else
					throw new InstantiationException();

			} catch (InstantiationException e) {
				logger.error("Can not instantiate abner, please"
						+ " check the mode" + "\n" + e.toString());
			}
		}
		abnerTagger.setTokenization(toBeTolkenized);

		logger.debug("Execute Started");
		// StringBuffer abnerInput = new StringBuffer();
		/*
		 * i needed those for BioCreative String abnerStringInput = null; String
		 * abnerOutput = null;
		 */

		try {
			AnnotationSet annotations;
			if (outputASName != null && outputASName.length() > 0)
				annotations = document.getAnnotations(outputASName);
			else
				annotations = document.getAnnotations();
			/*
			 * This is for custom tolkenization of the resource not needed for
			 * now
			 */
			/*
			 * if(toBeTolkenized) { Set<String> setAnn = new HashSet<String>();
			 * setAnn.add(TOKEN); setAnn.add(SPLIT);
			 * 
			 * AnnotationSet tokenAnnotations = annotations.get(setAnn); if
			 * (tokenAnnotations == null) return;
			 * 
			 * 
			 * List<Annotation> tokens = new ArrayList<Annotation>(tokenAnnotations);
			 * if(tokens.isEmpty()) return;
			 * 
			 * Collections.sort(tokens, new OffsetTypeComparator());
			 * 
			 * Annotation currToken = null; for (int i=0; i<tokens.size();i++){
			 * currToken = tokens.get(i); if(currToken.getType().equals(TOKEN))
			 * abnerInput.append(currToken.getFeatures().get("string") + " ");
			 * else abnerInput.append("\n"); } }
			 */

			logger.debug("Annotation loaded");

			Vector<String[][]> tagged = abnerTagger
					.getSegments(removeNonASCII(document.getContent()
							.toString()));

			for (Iterator iter = tagged.iterator(); iter.hasNext();) {
				// each element contain sentence
				String[][] element = (String[][]) iter.next();

				for (int token = 0; token < element[1].length; token++) {

					String phrase = element[0][token];
					String annotation = element[1][token];

					int offset[] = findLength(phrase);

					long startIndex = gateDocumentIndex;
					
					if (annotation.equals("PROTEIN")) {
						FeatureMap fm = Factory.newFeatureMap();

						// type
						fm.put("source", "abner");
						// source
						fm
								.put("type",
										"Gene");

						annotations.add((startIndex - offset[1]) + offset[0],
								gateDocumentIndex, "Tagger", fm);
					} else if (annotation.equals("CELL_LINE")) {
						FeatureMap fm = Factory.newFeatureMap();
						// fm.put("classid", "CELL_LINE");

						// type
						fm.put("source", "abner");
						// source
						fm
								.put("class",
										"CellLine");
						annotations.add((startIndex - offset[1]) + offset[0],
								gateDocumentIndex, "Tagger", fm);
					} else if (annotation.equals("CELL_TYPE")) {
						FeatureMap fm = Factory.newFeatureMap();
						// fm.put("classid", "CELL_TYPE");

						// type
						fm.put("source", "abner");
						// source
						fm
								.put("class",
										"CellType");
						annotations.add((startIndex - offset[1]) + offset[0],
								gateDocumentIndex, "Tagger", fm);
					} else if (annotation.equals("DNA")) {
						FeatureMap fm = Factory.newFeatureMap();
						// fm.put("classid", "DNA");

						// type
						fm.put("source", "abner");
						// source
						fm.put("class",
								"DNA");

						annotations.add((startIndex - offset[1]) + offset[0],
								gateDocumentIndex, "Tagger", fm);
					} else if (annotation.equals("RNA")) {
						FeatureMap fm = Factory.newFeatureMap();
						// fm.put("classid", "RNA");

						// type
						fm.put("source", "abner");
						// source
						fm.put("class",
								"RNA");

						annotations.add((startIndex - offset[1]) + offset[0],
								gateDocumentIndex, "Tagger", fm);
					}
				}
			}// end iterate over sentences

			logger.debug("Abner finished");

		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	private char[] compartibleChars = "[,.;:?!-+]{}\"`'()".toCharArray();

	private boolean isAbnerCompartible(char ch) {
		boolean bool = false;
		for (char element : compartibleChars) {
			if (element == ch)
				bool = true;
		}
		return bool;
	}

	private String removeNonASCII(String document) {

		char[] charDoc = null;
		charDoc = document.toCharArray();
		char[] returnDoc = document.toCharArray();

		for (int i = 0; i < charDoc.length; i++) {
			if (isAbnerCompartible(charDoc[i])) {
				returnDoc[i] = charDoc[i];
			} else if (Character.getNumericValue(charDoc[i]) > 255
					|| Character.getNumericValue(charDoc[i]) < 0) {
				returnDoc[i] = ' ';
			} else {
				returnDoc[i] = charDoc[i];
			}
		}

		StringBuffer a = new StringBuffer();

		for (char c : returnDoc) {
			a.append(c);
		}

		return a.toString();
	}

	private boolean isASCII(char ch) {
		if (isAbnerCompartible(ch)) {
			return true;
		} else if (Character.getNumericValue(ch) > 255
				|| Character.getNumericValue(ch) < 0) {
			return false;
		} else {
			return true;
		}

	}

	private int[] findLength(String phrase) {
		int length = 0;
		int endIndex = 0;

		long phraseSize = (long) (200 + phrase.length());
		endIndex = (int) ((document.getContent().size() - gateDocumentIndex) < phraseSize ? phrase
				.length()
				+ ((document.getContent().size() - gateDocumentIndex) - (long) phrase
						.length())
				: phrase.length() + 200);
		//

		char[] origSentence = new char[10];
		char[] parsePhrase = new char[10];

		try {
			origSentence = document.getContent().getContent(gateDocumentIndex,
					gateDocumentIndex + endIndex).toString().toCharArray();

			parsePhrase = phrase.toCharArray();

		} catch (InvalidOffsetException e) {
			logger.error("The Gate document offset are not correct: " + e
					+ "\n");
		}
		// start to compare
		int gateIndex = 0;
		int parseIndex = 0;
		/* most probably true */
		boolean isStartWhite = false;
		int start = 0;
		if (Character.isWhitespace(origSentence[0]))
			isStartWhite = true;

		try {
			// to see indexes
			while (parseIndex < parsePhrase.length) {
				if (origSentence[gateIndex] != parsePhrase[parseIndex]) {
					if (Character.isWhitespace(origSentence[gateIndex])) {
						length++;
						gateIndex++;
						if (isStartWhite)
							start++;
					} else if (!isASCII(origSentence[gateIndex])) {
						length++;
						gateIndex++;
					} else if (parsePhrase[parseIndex] == ' ') {
						parseIndex++;
						// do nothing it was parsed
					} else {
						throw new IOException(
								"The parsed element is not interval but: "
										+ parsePhrase[parseIndex] + "\n");
					}

				} else {
					if (isStartWhite)
						isStartWhite = false;
					length++;
					gateIndex++;
					parseIndex++;
				}
			}

			gateDocumentIndex += gateIndex;
		} catch (IOException e) {
			logger.error("Error: " + e.toString());
		}
		int result[] = { start, length };

		return result;
	}

	/*
	 * This method calculate mistaken spaces in a sentence, the problem is that
	 * abner ignores them but GATE model do not
	 */
	/*
	 * private int calculateSpaces(AnnotationSet spacesAS){ //return exess
	 * number of space or return 0 int exess = 0;
	 * 
	 * List<Annotation> spaces = new ArrayList<Annotation>(spacesAS);
	 * if(spaces.isEmpty()) return 0;
	 * 
	 * Collections.sort(spaces, new OffsetComparator());
	 * 
	 * Annotation lastSpace = null; for (int i=0; i<spaces.size();i++){
	 * Annotation currSpace = spaces.get(i); if (lastSpace != null){
	 * if(lastSpace.getEndNode().getOffset() ==
	 * currSpace.getStartNode().getOffset()) exess++; } lastSpace = currSpace; }
	 * 
	 * return exess; }
	 */

	/*
	 * private String removeIDs(String document){
	 * 
	 * StringBuffer returnDoc = new StringBuffer(); Reader myReader = new
	 * StringReader(document); BufferedReader bfReader = new
	 * BufferedReader(myReader);
	 * 
	 * try{
	 * 
	 * String line = null; while ((line = bfReader.readLine())!=null){
	 * if(!line.equals("")){ //the id must be parsed to the first interval, some
	 * are bigger //if (!line.endsWith(".") || !line.endsWith("?") ||
	 * !line.endsWith("!")) // returnDoc.append(line.substring(line.indexOf("
	 * ")+1, line.length()) + "\n\n");
	 * returnDoc.append(line.substring(line.indexOf(" ")+1, line.length()) +
	 * "\n"); //sentences.add(line.substring(line.indexOf(" ")+1,
	 * line.length())); //IDs.add(line.substring(0, line.indexOf(" "))); } }
	 * }catch(IOException e) { }
	 * 
	 * return returnDoc.toString(); }
	 * 
	 * //sentence level private List<AbnerAnnotation> compare (String document,
	 * String[] abner){ long start = 0L; long end = 0L; ArrayList ret = new
	 * ArrayList();
	 * 
	 * //analyze it symbol by symbol
	 * 
	 * //add all text parts from abner in separate instances ret.add(new
	 * AbnerAnnotation(start, end, AbnerAnnotationIDs.getID()));
	 * 
	 * 
	 * return ret; }
	 * 
	 * static void saveFile (String file, String content){ FileWriter
	 * tokenStream = null; BufferedWriter tokenIn = null;
	 * 
	 * 
	 * try{ tokenStream = new FileWriter(file); tokenIn = new
	 * BufferedWriter(tokenStream);
	 * 
	 * tokenIn.write(content);
	 * 
	 * 
	 * }catch (IOException e){ System.err.println("Error: the serilization
	 * lexicon file is missing" + e.getMessage()); } finally { if (tokenIn !=
	 * null) try {tokenIn.close();} catch (IOException e) {} } }
	 * 
	 * //Running the resource//
	 * 
	 * public static class AbnerAnnotationIDs {
	 * 
	 * private static long id;
	 * 
	 * public static long getID (){ return id++; }
	 * 
	 * private AbnerAnnotationIDs(){ id = 0; } }
	 * 
	 * public class AbnerAnnotation {
	 * 
	 * private long ID = 0;
	 * 
	 * AbnerAnnotation(long start, long end, long ID){ this.start = start;
	 * this.end = end; this.ID = ID; }
	 * 
	 * public long getID (){ return ID; }
	 * 
	 * public long getStart (){ return start; }
	 * 
	 * public long getEnd (){ return end; }
	 * 
	 * 
	 * private long start = 0; private long end = 0; }
	 */

	/**
	 * 
	 * @author georgiev implements comparator, compares two annotations on
	 *         offsets and type, type is returned desc
	 */
	public class OffsetTypeComparator implements Comparator {

		public int compare(Object o1, Object o2) {
			Annotation a1 = (Annotation) o1;
			Annotation a2 = (Annotation) o2;
			int result;

			// compare start offsets
			result = a1.getStartNode().getOffset().compareTo(
					a2.getStartNode().getOffset());

			// if start offsets are equal compare end offsets
			if (result == 0) {
				result = a1.getEndNode().getOffset().compareTo(
						a2.getEndNode().getOffset());
			} // if

			if (result == 0) {
				result = a1.getType().compareTo(a2.getType());
				// ivert the natural order, split after token wanted
				return result * -1;
			}

			return result;
		}
	}
}
