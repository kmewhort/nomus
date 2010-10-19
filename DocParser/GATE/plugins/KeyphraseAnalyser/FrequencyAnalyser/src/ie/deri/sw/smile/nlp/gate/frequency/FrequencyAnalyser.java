/*
 * 
 * FrequencyAnalyser.java, provides frequency analysis as a GATE plugin
 * Copyright (C) 2008  Alexander Schutz
 * National University of Ireland, Galway
 * Digital Enterprise Research Institute
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

package ie.deri.sw.smile.nlp.gate.frequency;

import gate.Annotation;
import gate.AnnotationSet;
import gate.Resource;
import gate.creole.ANNIEConstants;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import gate.util.Out;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class is the implementation of the resource FREQUENCYANALYSER.
 * <p>
 * it needs the language identification and tokenisation (and possibly
 * lemmatisation/stemming/morphological reduction) to be run before it can
 * handle annotations
 * <p>
 * tokens are matched via simple lookup in dedicated lists
 */
public class FrequencyAnalyser extends AbstractLanguageAnalyser {

	private static final long serialVersionUID = 328616036687300318L;

	private static Logger logger = Logger.getLogger(FrequencyAnalyser.class
			.getName());

	private static final String TOKEN_KIND_FEATURE_VALUE_WORD = "word";

	public FrequencyAnalyser() {
	}

	public Resource init() throws ResourceInstantiationException {

		return this;
	}

	public void execute() throws ExecutionException {
		try {

			if (document == null) {
				throw new ExecutionException("no document to process..");
			}

			FrequencyBearer fb = countTokens();

			if (fb.getTokenFreqM() != null) {
				document.getFeatures().put(tokenFrequencyFeatureName,
						fb.getTokenFreqM());
				// printTopTokens(25);
			}

			if (fb.getRootFreqM() != null) {
				document.getFeatures().put(lemmaFrequencyFeatureName,
						fb.getRootFreqM());
				// printTopLemmas(25);
			}

			if (fb.getNounlemmaFreqM() != null) {
				document.getFeatures().put(lemmaNounFrequencyFeatureName,
						fb.getNounlemmaFreqM());
				// printTopNounLemmas(25);
			}

			if (fb.getVerblemmaFreqM() != null) {
				document.getFeatures().put(lemmaVerbFrequencyFeatureName,
						fb.getVerblemmaFreqM());
				// printTopVerbLemmas(25);
			}

			if (fb.getAdjlemmaFreqM() != null) {
				document.getFeatures().put(lemmaAdjectiveFrequencyFeatureName,
						fb.getAdjlemmaFreqM());
				// printTopAdjLemmas(25);
			}

			if (fb.getDocumentSize() != -1) {
				document.getFeatures().put(documentSizeFeatureName,
						new Integer(fb.getDocumentSize()));
			}

			if (fb.getLexiconSize() != -1) {
				document.getFeatures().put(lexiconSizeFeatureName,
						new Integer(fb.getLexiconSize()));
			}

		} catch (RuntimeException exn) {
			throw new ExecutionException(exn);
		}

	}

	private FrequencyBearer countTokens() throws RuntimeException {

		long starttime = System.currentTimeMillis();

		Map<String, Integer> tokenFreqMap = new TreeMap<String, Integer>();
		Map<String, Integer> rootFreqMap = new TreeMap<String, Integer>();
		Map<String, Integer> posFreqMap = new TreeMap<String, Integer>();
		Map<String, Integer> nounFreqMap = new TreeMap<String, Integer>();
		Map<String, Integer> verbFreqMap = new TreeMap<String, Integer>();
		Map<String, Integer> adjFreqMap = new TreeMap<String, Integer>();

		FrequencyBearer fb = new FrequencyBearer();

		// get the annotationSet name provided by the user, or
		// otherwise use the default method
		AnnotationSet inputAS = (annotationSetName == null || annotationSetName
				.length() == 0) ? document.getAnnotations() : document
				.getAnnotations(annotationSetName);

		AnnotationSet tokens = inputAS.get(TOKEN_ANNOTATION_TYPE);

		if (tokens == null || tokens.isEmpty()) {

			throw new RuntimeException(document.getName()
					+ " does not have any contents or run the Tokeniser first");
		}

		Iterator<Annotation> tokenIter = tokens.iterator();
		// process tokens
		while (tokenIter != null && tokenIter.hasNext()) {
			Annotation currentToken = tokenIter.next();

			// only count words, omit non-words such as punctuation,
			// parenthesis, etc
			if (currentToken.getFeatures().get(TOKEN_KIND_FEATURE_NAME).equals(
					TOKEN_KIND_FEATURE_VALUE_WORD)) {

				// get Token string
				if (currentToken.getFeatures().containsKey(
						TOKEN_STRING_FEATURE_NAME)) {

					String tokenString = (String) currentToken.getFeatures()
							.get(TOKEN_STRING_FEATURE_NAME);

					String tokenStringLC = tokenString.toLowerCase();

					if (tokenFreqMap.containsKey(tokenStringLC)) {
						// increment counts
						int i = tokenFreqMap.get(tokenStringLC).intValue();
						tokenFreqMap.put(tokenStringLC, new Integer(++i));
					} else {
						// init with 1
						tokenFreqMap.put(tokenStringLC, new Integer(1));
					}

					tokenString = null;
					tokenStringLC = null;
				}

				// get lemma string for token in case it is there
				if (currentToken.getFeatures().containsKey(lemmaFeatureName)) {

					String rootString = (String) currentToken.getFeatures()
							.get(lemmaFeatureName);

					// we take the lower-cased string-value in case the
					// lemmatizer gives us <unknown> as analysis (quite often)
					if (rootString.equals(unknownLemmaValue)) {
						rootString = (String) currentToken.getFeatures().get(
								TOKEN_STRING_FEATURE_NAME);
					}

					String rootStringLC = rootString.toLowerCase();

					if (rootFreqMap.containsKey(rootStringLC)) {
						// increment counts
						int i = rootFreqMap.get(rootStringLC).intValue();
						rootFreqMap.put(rootStringLC, new Integer(++i));
					} else {
						// init with 1
						rootFreqMap.put(rootStringLC, new Integer(1));
					}

					rootString = null;
					rootStringLC = null;
				}

				// get parts-of-speech and fill in respective frequency maps
				if (currentToken.getFeatures().containsKey(
						coarseCategoryFeatureName)
						&& currentToken.getFeatures().containsKey(
								lemmaFeatureName)) {

					String rootString = (String) currentToken.getFeatures()
							.get(lemmaFeatureName);

					// we take the lower-cased string-value in case the
					// lemmatizer gives us <unknown> as analysis (quite often)
					if (rootString.equals(unknownLemmaValue)) {
						rootString = (String) currentToken.getFeatures().get(
								TOKEN_STRING_FEATURE_NAME);
					}

					String rootStringLC = rootString.toLowerCase();

					String coarseCategory = (String) currentToken.getFeatures()
							.get(coarseCategoryFeatureName);

					COARSE_POS_TAG_SET coarse_pos = mapToCoarseCategory(coarseCategory);
					if (coarse_pos != null) {
						switch (coarse_pos) {

						case NOUN:
							if (nounFreqMap.containsKey(rootStringLC)) {
								// increment counts
								int i = nounFreqMap.get(rootStringLC)
										.intValue();
								nounFreqMap.put(rootStringLC, new Integer(++i));
							} else {
								// init with 1
								nounFreqMap.put(rootStringLC, new Integer(1));
							}
							break;
						case VERB:
							if (verbFreqMap.containsKey(rootStringLC)) {
								// increment counts
								int i = verbFreqMap.get(rootStringLC)
										.intValue();
								verbFreqMap.put(rootStringLC, new Integer(++i));
							} else {
								// init with 1
								verbFreqMap.put(rootStringLC, new Integer(1));
							}
							break;
						case ADJECTIVE:
							if (adjFreqMap.containsKey(rootStringLC)) {
								// increment counts
								int i = adjFreqMap.get(rootStringLC).intValue();
								adjFreqMap.put(rootStringLC, new Integer(++i));
							} else {
								// init with 1
								adjFreqMap.put(rootStringLC, new Integer(1));
							}
							break;
						default:
							break;
						}
					}

					if (posFreqMap.containsKey(coarseCategory)) {
						// increment counts
						int i = posFreqMap.get(coarseCategory).intValue();
						posFreqMap.put(coarseCategory, new Integer(++i));
					} else {
						// init with 1
						posFreqMap.put(coarseCategory, new Integer(1));
					}

				}

			}
			currentToken = null;
		}

		tokenIter = null;

		logger.log(Level.INFO, "finished counting, elapsed time: "
				+ (System.currentTimeMillis() - starttime) + "ms");

		// if (!tokenFreqMap.isEmpty()) {
		fb.setTokenFreqM(sortMapByFrequency(tokenFreqMap));
		// }

		// if (!rootFreqMap.isEmpty()) {
		fb.setRootFreqM(sortMapByFrequency(rootFreqMap));
		// }

		// if (!nounFreqMap.isEmpty()) {
		fb.setNounlemmaFreqM(sortMapByFrequency(nounFreqMap));
		// }

		// if (!verbFreqMap.isEmpty()) {
		fb.setVerblemmaFreqM(sortMapByFrequency(verbFreqMap));
		// }

		// if (!adjFreqMap.isEmpty()) {
		fb.setAdjlemmaFreqM(sortMapByFrequency(adjFreqMap));
		// }

		// if (!posFreqMap.isEmpty()) {
		fb.setPosFreqM(sortMapByFrequency(posFreqMap));
		// }

		// clean up
		tokenFreqMap.clear();
		tokenFreqMap = null;
		rootFreqMap.clear();
		rootFreqMap = null;

		return fb;
	}

	private Map<String, Integer> sortMapByFrequency(Map<String, Integer> aMap) {

		List<Map.Entry<String, Integer>> eList = new ArrayList<Map.Entry<String, Integer>>(
				aMap.entrySet());

		Collections.sort(eList, new Comparator<Map.Entry<String, Integer>>() {
			public int compare(Entry<String, Integer> arg0,
					Entry<String, Integer> arg1) {
				if (arg0.getValue().intValue() > arg1.getValue().intValue()) {
					return -1;
				} else if (arg0.getValue().intValue() < arg1.getValue()
						.intValue()) {
					return 1;
				} else {
					return arg0.getKey().compareToIgnoreCase(arg1.getKey());
				}
			}
		});

		Map<String, Integer> sortedMap = new LinkedHashMap<String, Integer>();
		Iterator<Map.Entry<String, Integer>> entryItr = eList.iterator();
		while (entryItr.hasNext()) {
			Map.Entry<String, Integer> e = entryItr.next();
			sortedMap.put(e.getKey(), e.getValue());
		}

		// clean up
		entryItr = null;
		eList.clear();
		eList = null;

		return sortedMap;
	}

	private COARSE_POS_TAG_SET mapToCoarseCategory(String coarse_category) {

		if (coarse_category
				.equalsIgnoreCase(COARSE_POS_TAG_SET.NOUN.toString())) {
			return COARSE_POS_TAG_SET.NOUN;
		} else if (coarse_category.equalsIgnoreCase(COARSE_POS_TAG_SET.VERB
				.toString())) {
			return COARSE_POS_TAG_SET.VERB;
		} else if (coarse_category
				.equalsIgnoreCase(COARSE_POS_TAG_SET.ADJECTIVE.toString())) {
			return COARSE_POS_TAG_SET.ADJECTIVE;
		} else {
			return null;
		}
	}

	private void printTopTokens(int topN) {

		Iterator<Map.Entry<String, Integer>> itr = ((Map<String, Integer>) document
				.getFeatures().get(tokenFrequencyFeatureName)).entrySet()
				.iterator();

		int i = 0;
		while (i < topN && itr.hasNext()) {
			Map.Entry<String, Integer> e = itr.next();
			Out.println(String.format("%1$ 4d : %2$20s -- %3$ 5d", i++, e
					.getKey(), e.getValue()));
		}
	}

	private void printTopLemmas(int topN) {

		Iterator<Map.Entry<String, Integer>> itr = ((Map<String, Integer>) document
				.getFeatures().get(lemmaFrequencyFeatureName)).entrySet()
				.iterator();

		int i = 0;
		while (i < topN && itr.hasNext()) {
			Map.Entry<String, Integer> e = itr.next();
			Out.println(String.format("%1$ 4d : %2$20s -- %3$ 5d", i++, e
					.getKey(), e.getValue()));
		}
	}

	private void printTopNounLemmas(int topN) {

		Iterator<Map.Entry<String, Integer>> itr = ((Map<String, Integer>) document
				.getFeatures().get(lemmaNounFrequencyFeatureName)).entrySet()
				.iterator();

		int i = 0;
		while (i < topN && itr.hasNext()) {
			Map.Entry<String, Integer> e = itr.next();
			Out.println(String.format("%1$ 4d : %2$20s -- %3$ 5d", i++, e
					.getKey(), e.getValue()));
		}
	}

	private void printTopVerbLemmas(int topN) {

		Iterator<Map.Entry<String, Integer>> itr = ((Map<String, Integer>) document
				.getFeatures().get(lemmaVerbFrequencyFeatureName)).entrySet()
				.iterator();

		int i = 0;
		while (i < topN && itr.hasNext()) {
			Map.Entry<String, Integer> e = itr.next();
			Out.println(String.format("%1$ 4d : %2$20s -- %3$ 5d", i++, e
					.getKey(), e.getValue()));
		}
	}

	private void printTopAdjLemmas(int topN) {

		Iterator<Map.Entry<String, Integer>> itr = ((Map<String, Integer>) document
				.getFeatures().get(lemmaAdjectiveFrequencyFeatureName))
				.entrySet().iterator();

		int i = 0;
		while (i < topN && itr.hasNext()) {
			Map.Entry<String, Integer> e = itr.next();
			Out.println(String.format("%1$ 4d : %2$20s -- %3$ 5d", i++, e
					.getKey(), e.getValue()));
		}
	}

	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

	public String getEncoding() {
		return this.encoding;
	}

	/**
	 * Set the document to process.
	 */
	public void setDocument(gate.Document document) {
		this.document = document;
	}

	/**
	 * Return the document being processed.
	 */
	public gate.Document getDocument() {
		return document;
	}

	/**
	 * Set the name of the annotation set to place the generated Token
	 * annotations in.
	 */
	public void setAnnotationSetName(String annotationSetName) {
		this.annotationSetName = annotationSetName;
	}

	/**
	 * Return the annotation set name used for the Tokens.
	 */
	public String getAnnotationSetName() {
		return annotationSetName;
	}

	public void setResourceDir(java.net.URL resourceDir) {
		this.resourceDir = resourceDir;
	}

	public java.net.URL getResourceDir() {
		return this.resourceDir;
	}

	public String getCoarseCategoryFeatureName() {
		return coarseCategoryFeatureName;
	}

	public void setCoarseCategoryFeatureName(String coarseCategoryFeatureName) {
		this.coarseCategoryFeatureName = coarseCategoryFeatureName;
	}

	public String getLemmaFeatureName() {
		return lemmaFeatureName;
	}

	public void setLemmaFeatureName(String lemmaFeatureName) {
		this.lemmaFeatureName = lemmaFeatureName;
	}

	public String getTokenFrequencyFeatureName() {
		return tokenFrequencyFeatureName;
	}

	public void setTokenFrequencyFeatureName(String tokenFrequencyFeatureName) {
		this.tokenFrequencyFeatureName = tokenFrequencyFeatureName;
	}

	public String getLemmaFrequencyFeatureName() {
		return lemmaFrequencyFeatureName;
	}

	public void setLemmaFrequencyFeatureName(String lemmaFrequencyFeatureName) {
		this.lemmaFrequencyFeatureName = lemmaFrequencyFeatureName;
	}

	public String getLemmaNounFrequencyFeatureName() {
		return lemmaNounFrequencyFeatureName;
	}

	public void setLemmaNounFrequencyFeatureName(
			String lemmaNounFrequencyFeatureName) {
		this.lemmaNounFrequencyFeatureName = lemmaNounFrequencyFeatureName;
	}

	public String getLemmaVerbFrequencyFeatureName() {
		return lemmaVerbFrequencyFeatureName;
	}

	public void setLemmaVerbFrequencyFeatureName(
			String lemmaVerbFrequencyFeatureName) {
		this.lemmaVerbFrequencyFeatureName = lemmaVerbFrequencyFeatureName;
	}

	public String getLemmaAdjectiveFrequencyFeatureName() {
		return lemmaAdjectiveFrequencyFeatureName;
	}

	public void setLemmaAdjectiveFrequencyFeatureName(
			String lemmaAdjectiveFrequencyFeatureName) {
		this.lemmaAdjectiveFrequencyFeatureName = lemmaAdjectiveFrequencyFeatureName;
	}

	public String getDocumentSizeFeatureName() {
		return documentSizeFeatureName;
	}

	public void setDocumentSizeFeatureName(String documentSizeFeatureName) {
		this.documentSizeFeatureName = documentSizeFeatureName;
	}

	public String getLexiconSizeFeatureName() {
		return lexiconSizeFeatureName;
	}

	public void setLexiconSizeFeatureName(String lexiconSizeFeatureName) {
		this.lexiconSizeFeatureName = lexiconSizeFeatureName;
	}

	public void setUnknownLemmaValue(String unknownLemmaValue) {
		this.unknownLemmaValue = unknownLemmaValue;
	}

	public String getUnknownLemmaValue() {
		return this.unknownLemmaValue;
	}

	private String encoding;
	private java.net.URL resourceDir;
	private String annotationSetName;
	private gate.Document document;
	private String coarseCategoryFeatureName;
	private String lemmaFeatureName;
	private String tokenFrequencyFeatureName;
	private String lemmaFrequencyFeatureName;
	private String lemmaNounFrequencyFeatureName;
	private String lemmaVerbFrequencyFeatureName;
	private String lemmaAdjectiveFrequencyFeatureName;
	private String documentSizeFeatureName;
	private String lexiconSizeFeatureName;
	private String unknownLemmaValue;

} // class FrequencyAnalyser

enum COARSE_POS_TAG_SET {
	NOUN, VERB, ADJECTIVE, ADVERB, DETERMINER_OR_PRONOUN
}
