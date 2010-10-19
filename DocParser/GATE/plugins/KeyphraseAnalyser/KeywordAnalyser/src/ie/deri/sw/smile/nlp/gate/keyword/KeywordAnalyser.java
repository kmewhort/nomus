/*
 * 
 * KeywordAnalyser.java, provides keyword/keyphrase extraction as a GATE plugin
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

package ie.deri.sw.smile.nlp.gate.keyword;

import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import ie.deri.sw.smile.nlp.gate.keyword.util.FrequencyListReader;
import ie.deri.sw.smile.nlp.gate.keyword.util.NormalisedFrequencyBearer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;

/**
 * This class is the implementation of the resource KEYWORDANALYSER.
 */
public class KeywordAnalyser extends AbstractLanguageAnalyser {

	private static final long serialVersionUID = 1028964445148413481L;

	private static Logger logger = Logger.getLogger(KeywordAnalyser.class
			.getName());

	// public static String NOUNCHUNK_ANNOTATION_TYPE;

	public static final String TOKEN_KIND_FEATURE_VALUE_WORD = "word";

	private static final String FREQUENCYLIST_FILE_PREFIX = "frequency_";

	private static final String FREQUENCYLIST_FILE_SUFFIX = ".num.gz";

	public static final int MIN_DOC_SIZE_FOR_REASONABLE_PROCESSING = 500;

	private Map<String, Integer> refFrequencyMap = null;
	private int refLexiconSize = -1;
	private int refCorpusSize = -1;
	private String lastLanguage = null;

	public KeywordAnalyser() {
		refFrequencyMap = new HashMap<String, Integer>();
	}

	/*
	 * DONE init wordlists/lemma per document (saves memory, and ensures only
	 * one list is stored in memory at any given time)
	 * 
	 * retrieve frequency lists
	 * 
	 * based on language AND frequency observation,
	 * 
	 * TODO compute different statistics for single token occurrences, which
	 * gives us cue-words
	 * 
	 * look up cue-words in their context (noun chunks for the time being)
	 * 
	 * put into visual resource for displaying
	 * 
	 */
	public Resource init() throws ResourceInstantiationException {

		if (resourceDir == null) {
			throw new ResourceInstantiationException(
					"no resource directory provided..");
		}
		// check whether file conforms to naming conventions
		java.io.File[] frequencyFiles = new java.io.File(resourceDir.getFile())
				.listFiles(new java.io.FileFilter() {

					public boolean accept(File pathname) {
						String filename = pathname.getName();
						if (filename.startsWith(FREQUENCYLIST_FILE_PREFIX)
								&& filename.endsWith(FREQUENCYLIST_FILE_SUFFIX)) {
							return true;
						}
						return false;
					}
				});

		// see what is available, do not load yet:
		StringBuilder languages = new StringBuilder();
		for (int i = 0; i < frequencyFiles.length; i++) {

			String language = frequencyFiles[i].getName().substring(10, 12);

			// storing populated stopword set in hash with language as key
			languages.append(language).append(",");

		}
		logger.log(Level.INFO, KeywordAnalyser.class.getName()
				+ ": frequency lists available for following languages: "
				+ languages.subSequence(0, languages.length() - 1).toString());

		return this;
	}

	public void execute() throws ExecutionException {
		try {

			if (document == null) {
				throw new ExecutionException("no document to process..");
			}

			if (!document.getFeatures().containsKey(languageFeatureName)) {
				throw new ExecutionException(
						"no language parameter given, cannot compute statistical tests, only extracting frequency based keywords..");
			}

			if (!document.getFeatures().containsKey(lemmaFrequencyFeatureName)) {
				throw new ExecutionException(
						"no frequency analysis available, cannot extract keywords, run Frequency Analyser first");
			}

			readFrequencyList((String) document.getFeatures().get(
					languageFeatureName));

			/*
			 * ################################################################
			 * the procedure is as follows: <p> 1. determine
			 * relevance/significance on a lexical basis, and use the outcome as
			 * seed input to identify larger chunks. we use LexicalStrategy to
			 * achieve that. <p> 2. with seed input (Map<String,Float>) we
			 * extract either NounChunks or {2,3,4}-grams, depeding on
			 * availability of Chunker ProcessingResource for given language.
			 * Containing input seed elements. produce Map<DescriptiveTerm,Integer>,
			 * where
			 * DescriptiveTerm:{tokenString,rootString,equals()-based-on-rootString},
			 * Integer:frequency <p> 3. from that map, produce Map<Keyword,Float>
			 * such that Keyword assumes the form of the most dominantly
			 * occurring shape in DescriptiveTerm, and Float is a confidence
			 * value. Float will decrease with more variation in
			 * DescriptiveTerm{tokenString}, and will also decrease with
			 * decreasing frequency. Having multiple tokens of seed input in one
			 * DescriptiveTerm raises Float
			 */

			// 1. this is actually a bit more complex: we must determine whether
			// it makes
			// sense to use statistical measures or simply bear with frequency.
			// if we
			// have consistently more than 5 observations, we use statistical
			// measures,
			// otherwise we bear with frequency of the topN items
			LexicalStrategy lexicalStrategy = StrategyFactory
					.getLexicalStrategy(document);
			LexicalCandidateStore lexicalCandidateStore = lexicalStrategy
					.produceCandidateTerms(refCorpusSize, refFrequencyMap);

			// lexicalCandidateStore.print();

			LinkedHashMap<String, Double> keywordMap = new LinkedHashMap<String, Double>();

			// if document is very small we apply this evil hack
			if (((Integer) document.getFeatures().get(
					this.documentSizeFeatureName)).intValue() < this.MIN_DOC_SIZE_FOR_REASONABLE_PROCESSING) {

				assembleKeywordsForVeryShortText(keywordMap,
						lexicalCandidateStore);

			} else {
				// assemble larger units here

				// need to settle on correct strategy again, this time depending
				// on whether we have larger units available (chunks or
				// phrases),
				// if not, we have to construct n-grams (n=[2;4[)
				ComplexUnitStrategy complexUnitStrategy = StrategyFactory
						.getComplexUnitStrategy(document);
				Set<ComplexTerm> complexTermSet = complexUnitStrategy
						.produceCandidateTerms(lexicalCandidateStore);

				// for (ComplexTerm ct : complexTermSet ){
				// System.out.println( ct.getStringValue());
				// }

				// perform analysis over ComplexTerms such that we are able to
				// identify
				// KeywordOrPhraseCandidates therein
				ComplexTermAnalyser ctAnalyser = new ComplexTermAnalyser(
						document);
				SortedSet<KeywordOrPhraseCandidate> keywordOrPhraseSet = ctAnalyser
						.analyse(complexTermSet);

				// adding keyword candidates to resulting KeywordMap which is to
				// be stored as document feature
				Iterator<KeywordOrPhraseCandidate> kwItr = keywordOrPhraseSet
						.iterator();
				while (kwItr.hasNext()) {
					KeywordOrPhraseCandidate kw = kwItr.next();
					// kw.print();
					keywordMap.put(kw.getStringValue(), new Double(kw
							.getConfidence()));
					kw = null;
				}
				keywordOrPhraseSet.clear();
				keywordOrPhraseSet = null;
				complexTermSet.clear();
				complexTermSet = null;
			}

			document.getFeatures().put(keywordFeatureName, keywordMap);

		} catch (IOException exn) {
			throw new ExecutionException(exn);
		} catch (RuntimeException exn) {
			throw new ExecutionException(exn);
		}

	}

	private void assembleKeywordsForVeryShortText(
			Map<String, Double> keywordMap,
			LexicalCandidateStore lexicalCandidateStore) {

		int maxFrequency = 0;
		for (LexicalCandidate lexCand : lexicalCandidateStore.getFrequencySet()) {
			maxFrequency += lexCand.getFrequency();
		}
		for (LexicalCandidate lexCand : lexicalCandidateStore.getFrequencySet()) {
			keywordMap.put(lexCand.getStringValue(), new Double( (double)lexCand.getFrequency() / (double)maxFrequency ));
		}

		// 
		double maxRelevance = 0;
		for (LexicalCandidate lexCand : lexicalCandidateStore.getRelevanceSet()) {
			maxRelevance += lexCand.getFrequency();
		}
		for (LexicalCandidate lexCand : lexicalCandidateStore.getRelevanceSet()) {
			keywordMap.put(lexCand.getStringValue(), new Double( lexCand.getRelevance() / maxRelevance ));
		}

	}

	private void readFrequencyList(String language) throws IOException {

		// read freq list
		File frequencyListFile = new java.io.File(resourceDir.getFile(),
				FREQUENCYLIST_FILE_PREFIX
						+ (String) document.getFeatures().get(
								languageFeatureName)
						+ FREQUENCYLIST_FILE_SUFFIX);
		FrequencyListReader freqFileReader = null;
		long start = System.currentTimeMillis();
		try {
			freqFileReader = new FrequencyListReader(
					new InputStreamReader(new GZIPInputStream(
							new FileInputStream(frequencyListFile))));

			this.refCorpusSize = freqFileReader.getCorpusSize();
			this.refLexiconSize = freqFileReader.getLexiconSize();

			while (freqFileReader.ready()) {

				NormalisedFrequencyBearer nfb = freqFileReader
						.readFrequencyLine();

				refFrequencyMap.put(nfb.getToken(), nfb.getRealFrequency());

				nfb = null;
			}

		} catch (IOException exn) {
			logger.log(Level.WARNING,
					"a problem occured while reading the frequency list: "
							+ frequencyListFile.getAbsolutePath(), exn);
		} finally {
			try {
				freqFileReader.close();
			} catch (IOException exn) {
				exn.printStackTrace();
			}
			logger.log(Level.INFO, "reference frequency list loaded in "
					+ (System.currentTimeMillis() - start) + "ms ..");
			// set language
			lastLanguage = (String) document.getFeatures().get(
					languageFeatureName);
		}

	}

	private void simpleFrequencyComparison() {
		System.out.println("reference corpus size: " + refCorpusSize);
		System.out.println("reference lexicon size: " + refLexiconSize);
		Iterator<Map.Entry<String, Integer>> docFreqItr = ((Map<String, Integer>) document
				.getFeatures().get(lemmaFrequencyFeatureName)).entrySet()
				.iterator();
		int i = 0;
		while (docFreqItr.hasNext() && i++ < 25) {
			Map.Entry<String, Integer> docTokenFreq = docFreqItr.next();
			System.out
					.println(docTokenFreq.getKey()
							+ " -- "
							+ docTokenFreq.getValue()
							+ " -- in flist: "
							+ (refFrequencyMap.containsKey(docTokenFreq
									.getKey()) ? refFrequencyMap
									.get(docTokenFreq.getKey()) : 0));

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

	public String getUnknownLemmaValue() {
		return unknownLemmaValue;
	}

	public void setUnknownLemmaValue(String unknownLemmaValue) {
		this.unknownLemmaValue = unknownLemmaValue;
	}

	public String getLanguageFeatureName() {
		return languageFeatureName;
	}

	public void setLanguageFeatureName(String languageFeatureName) {
		this.languageFeatureName = languageFeatureName;
	}

	public String getNounchunkAnnotationType() {
		return nounchunkAnnotationType;
	}

	public void setNounchunkAnnotationType(String nounchunkAnnotationType) {
		this.nounchunkAnnotationType = nounchunkAnnotationType;
	}

	public void setKeywordFeatureName(String keywordFeatureName) {
		this.keywordFeatureName = keywordFeatureName;
	}

	public String getKeywordFeatureName() {
		return keywordFeatureName;
	}

	public void setStopwordFeatureName(String stopwordFeatureName) {
		this.stopwordFeatureName = stopwordFeatureName;
	}

	public String getStopwordFeatureName() {
		return stopwordFeatureName;
	}

	private String encoding;
	private java.net.URL resourceDir;
	private String annotationSetName;
	private gate.Document document;
	private String languageFeatureName;
	static String coarseCategoryFeatureName;
	static String nounchunkAnnotationType;
	static String lemmaFeatureName;
	static String stopwordFeatureName;
	static String tokenFrequencyFeatureName;
	static String lemmaFrequencyFeatureName;
	static String lemmaNounFrequencyFeatureName;
	static String lemmaVerbFrequencyFeatureName;
	static String lemmaAdjectiveFrequencyFeatureName;
	static String documentSizeFeatureName;
	static String lexiconSizeFeatureName;
	static String unknownLemmaValue;
	static String keywordFeatureName;

} // class KeywordAnalyser

enum COARSE_POS_TAG_SET {
	NOUN, VERB, ADJECTIVE, ADVERB, DETERMINER_OR_PRONOUN
}
