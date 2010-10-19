/*
 * 
 * StrategyFactory.java, provides keyword/keyphrase extraction as a GATE plugin
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

import gate.creole.ANNIEConstants;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

public class StrategyFactory {

	private static final int CUT_OFF = 4;
	private static final int MIN_SIZE_CANDIDATE_LIST = 10;
	private static final int MIN_WORD_LENGTH = 2;
	private static final int NOUN_LIMIT = 25;
	private static final int VERB_LIMIT = 25;
	private static final int ADJ_LIMIT = 25;

	private static Logger logger = Logger.getLogger(StrategyFactory.class
			.getName());

	/**
	 * returns the appropriate strategy for dealing with/analysing lexical
	 * items, depending on frequency of observations.
	 * 
	 * @param document
	 * @return
	 */
	public static LexicalStrategy getLexicalStrategy(gate.Document document) {

		Set<Map.Entry<String, Integer>> filteredFreqSet = new HashSet<Map.Entry<String, Integer>>();
		Set<Map.Entry<String, Integer>> unfilteredFreqSet = new HashSet<Map.Entry<String, Integer>>();

		Set<Map.Entry<String, Integer>> freqSet = null;

		// System.out.println("Upper "+NOUNLIMIT+"% and > CUT_OFF of nouns");
		freqSet = ((Map<String, Integer>) document.getFeatures().get(
				KeywordAnalyser.lemmaNounFrequencyFeatureName)).entrySet();
		filteredFreqSet.addAll(getUpperQuarterOfLexicalTerms(freqSet, true,
				NOUN_LIMIT));
		unfilteredFreqSet.addAll(getUpperQuarterOfLexicalTerms(freqSet, false,
				NOUN_LIMIT));

		/* ################### WE KEEP IT SIMPLE FOR NOW ################## */
		// System.out.println("Upper "+VERBLIMIT+"% and > CUT_OFF of verbs");
		// freqSet = ((Map<String, Integer>) document.getFeatures().get(
		// KeywordAnalyser.lemmaVerbFrequencyFeatureName))
		// .entrySet();
		//
		// filteredFreqSet.addAll(getUpperQuarterOfLexicalTerms(freqSet, true,
		// VERB_LIMIT));
		// unfilteredFreqSet.addAll(getUpperQuarterOfLexicalTerms(freqSet,
		// false,
		// VERB_LIMIT));
		// System.out.println("Upper "+ADJLIMIT+"% and > CUT_OFF of adj");
		// freqSet = ((Map<String, Integer>) document.getFeatures().get(
		// KeywordAnalyser.lemmaAdjectiveFrequencyFeatureName))
		// .entrySet();
		// filteredFreqSet.addAll(getUpperQuarterOfLexicalTerms(freqSet, true,
		// ADJ_LIMIT));
		// unfilteredFreqSet.addAll(getUpperQuarterOfLexicalTerms(freqSet,
		// false,
		// ADJ_LIMIT));
		freqSet = null;

		// 
		if (filteredFreqSet.size() > MIN_SIZE_CANDIDATE_LIST) {
			unfilteredFreqSet.clear();
			unfilteredFreqSet = null;
			return new LexicalStatisticsStrategy(
					((Integer) document.getFeatures().get(
							KeywordAnalyser.documentSizeFeatureName))
							.intValue(), filteredFreqSet);
		} else {
			filteredFreqSet.clear();
			filteredFreqSet = null;
			return new LexicalFrequencyStrategy(unfilteredFreqSet);
		}
	}

	/**
	 * returns the appropriate strategy for dealing with complex terms,
	 * depending on frequency of observations.
	 * 
	 * @param document
	 * @return
	 */
	public static ComplexUnitStrategy getComplexUnitStrategy(
			gate.Document document) {
		// need to settle on correct strategy again, this time depending
		// on whether we have larger units available (chunks or phrases),
		// if not, we have to construct n-grams (n=[2;4[)

		if (document.getAnnotations().get(
				KeywordAnalyser.nounchunkAnnotationType) != null
				&& !document.getAnnotations().get(
						KeywordAnalyser.nounchunkAnnotationType).isEmpty()) {
			return new ChunkStrategy(document.getAnnotations());
			// return new NGramStrategy(document.getAnnotations().get(
			// ANNIEConstants.TOKEN_ANNOTATION_TYPE));
		} else {
			return new NGramStrategy(document.getAnnotations().get(
					ANNIEConstants.TOKEN_ANNOTATION_TYPE));
		}

	}

	private static Set<Map.Entry<String, Integer>> getUpperQuarterOfLexicalTerms(
			Set<Map.Entry<String, Integer>> freqSet, boolean filterByCutOff,
			int max_items) {

		Set<Map.Entry<String, Integer>> filteredSet = new HashSet<Map.Entry<String, Integer>>();
		Iterator<Map.Entry<String, Integer>> setItr = freqSet.iterator();
		int i = 0;

		// we compute upper quarter here because we know we will always have a
		// large
		// amount (long tail distribution) of single occurring events
		int first_25pc = computeUpperQuarter(freqSet);
		while (setItr.hasNext() && i < first_25pc && i++ < max_items) {
			Map.Entry<String, Integer> termFreq = setItr.next();
			// if > cutoff then we can apply statistical measures
			if (filterByCutOff) {
				if (termFreq.getKey().length() >= MIN_WORD_LENGTH
						&& termFreq.getValue().intValue() > CUT_OFF) {
					filteredSet.add(termFreq);
				}
			} else {
				if (termFreq.getKey().length() >= MIN_WORD_LENGTH) {
					filteredSet.add(termFreq);
				}
			}
		}

		return filteredSet;
	}

	private static int computeUpperQuarter(Set set) {
		if (!set.isEmpty()) {
			return set.size() / 4;
		}

		return 0;
	}

}
