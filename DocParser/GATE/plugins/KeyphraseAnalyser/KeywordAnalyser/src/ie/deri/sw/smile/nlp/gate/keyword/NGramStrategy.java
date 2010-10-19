/*
 * 
 * NGramStrategy.java, provides keyword/keyphrase extraction as a GATE plugin
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

import gate.Annotation;
import gate.AnnotationSet;
import gate.FeatureMap;
import gate.creole.ANNIEConstants;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

public class NGramStrategy implements ComplexUnitStrategy {

	private static Logger logger = Logger.getLogger(NGramStrategy.class
			.getName());

	private AnnotationSet tokenAnnotations;

	private SortedMap<Long, Annotation> tokenAnnotationMap;

	static boolean containsNoun = false;

	static int numberNGramsSkipped = 0;

	public NGramStrategy(AnnotationSet tokenAnnotations) {
		this.tokenAnnotations = tokenAnnotations;

		this.tokenAnnotationMap = new TreeMap<Long, Annotation>();

		transformAnnotationSet2Map(tokenAnnotations, tokenAnnotationMap);

	};

	private void transformAnnotationSet2Map(AnnotationSet aSet,
			SortedMap<Long, Annotation> aMap) {
		Iterator<Annotation> aItr = aSet.iterator();
		while (aItr.hasNext()) {
			Annotation a = aItr.next();
			aMap.put(a.getStartNode().getOffset(), a);
		}
	}

	public Set<ComplexTerm> produceCandidateTerms(
			LexicalCandidateStore lexicalCandidateStore) {
		numberNGramsSkipped = 0;
		Set<ComplexTerm> complexTermSet = new LinkedHashSet<ComplexTerm>();

		long start = System.currentTimeMillis();

		// wrapping lexicalCandidateStore into List as described below
		List<LexicalCandidate> tmpLexCandStoreRelevanceSet = new ArrayList<LexicalCandidate>(
				lexicalCandidateStore.getRelevanceSet());
		List<LexicalCandidate> tmpLexCandStoreFrequencySet = new ArrayList<LexicalCandidate>(
				lexicalCandidateStore.getFrequencySet());

		List<Annotation> sortedTokenList = new ArrayList<Annotation>(
				tokenAnnotationMap.values());

		// we iterate !ONCE! over token annotations and compare root/lemma with
		// both sets, if contained, we build n-grams, and continue with
		// iteration

		// the result of a comparison between LexicalCandidates is determined by
		// the respective StringValues, so
		// we wrap every lemma of the iteration into a LexicalCandidate first
		// and test for containment in the set :
		// changed: because we need to retrieve the element to access the
		// significance value, we need to wrap the set into a list, we do this
		// out of the scope of this unfortunate loop
		for (int idx = 0; idx < sortedTokenList.size(); idx++) {

			if (sortedTokenList.get(idx).getFeatures().get(
					KeywordAnalyser.lemmaFeatureName).equals(
					KeywordAnalyser.unknownLemmaValue)) {

				// the unfortunate wrapping
				LexicalCandidate tmpWrappedLexCand = new LexicalCandidateImpl(
						(String) sortedTokenList.get(idx).getFeatures().get(
								KeywordAnalyser.TOKEN_STRING_FEATURE_NAME), 1);
				
				if (lexicalCandidateStore.getRelevanceSet().contains(
						tmpWrappedLexCand)) {

					// again, this ONLY works because we have overridden
					// hashCode and equals for LexicalCandidate
					double lexCandSignificance = tmpLexCandStoreRelevanceSet
							.get(tmpLexCandStoreRelevanceSet
									.indexOf(tmpWrappedLexCand)).getRelevance();

					for (int current_n = NGRAM_MIN_N; current_n <= NGRAM_MAX_N; current_n++) {
						// we have to figure that one out
						// dont need to consider sublist here, as we can access
						// the
						// elements in array list in O(1) time
						constructNGram(idx, current_n, sortedTokenList,
								complexTermSet);
					}
				}

			} else {
				// the unfortunate wrapping
				LexicalCandidate tmpWrappedLexCand = new LexicalCandidateImpl(
						(String) sortedTokenList.get(idx).getFeatures().get(
								KeywordAnalyser.lemmaFeatureName), 1);

				if (lexicalCandidateStore.getRelevanceSet().contains( tmpWrappedLexCand )) {

					// again, this ONLY works because we have overridden
					// hashCode and equals for LexicalCandidate
					double lexCandSignificance = tmpLexCandStoreRelevanceSet
							.get(tmpLexCandStoreRelevanceSet
									.indexOf(tmpWrappedLexCand)).getRelevance();

					for (int current_n = NGRAM_MIN_N; current_n <= NGRAM_MAX_N; current_n++) {
						// we have to figure that one out
						// dont need to consider sublist here, as we can access
						// the
						// elements in array list in O(1) time
						constructNGram(idx, current_n, sortedTokenList,
								complexTermSet);
					}
				}

			}

			// we do the same for the frequency list
			if (sortedTokenList.get(idx).getFeatures().get(
					KeywordAnalyser.lemmaFeatureName).equals(
					KeywordAnalyser.unknownLemmaValue)) {

				// the unfortunate wrapping
				LexicalCandidate tmpWrappedLexCand = new LexicalCandidateImpl(
						(String) sortedTokenList.get(idx).getFeatures().get(
								KeywordAnalyser.TOKEN_STRING_FEATURE_NAME), 1);

				if (lexicalCandidateStore.getFrequencySet().contains( tmpWrappedLexCand )) {

					// again, this ONLY works because we have overridden
					// hashCode and equals for LexicalCandidate
					double lexCandSignificance = tmpLexCandStoreFrequencySet
							.get(tmpLexCandStoreFrequencySet
									.indexOf(tmpWrappedLexCand)).getFrequency();

					for (int current_n = NGRAM_MIN_N; current_n <= NGRAM_MAX_N; current_n++) {
						// we have to figure that one out
						// dont need to consider sublist here, as we can access
						// the
						// elements in array list in O(1) time
						constructNGram(idx, current_n, sortedTokenList,
								complexTermSet);
					}
				}

			} else {

				// the unfortunate wrapping
				LexicalCandidate tmpWrappedLexCand = new LexicalCandidateImpl(
						(String) sortedTokenList.get(idx).getFeatures().get(
								KeywordAnalyser.TOKEN_STRING_FEATURE_NAME), 1);

				if (lexicalCandidateStore.getFrequencySet().contains( tmpWrappedLexCand )) {

					// again, this ONLY works because we have overridden
					// hashCode and equals for LexicalCandidate
					double lexCandSignificance = tmpLexCandStoreFrequencySet
							.get(tmpLexCandStoreFrequencySet
									.indexOf(tmpWrappedLexCand)).getFrequency();

					for (int current_n = NGRAM_MIN_N; current_n <= NGRAM_MAX_N; current_n++) {
						// we have to figure that one out
						// dont need to consider sublist here, as we can access
						// the
						// elements in array list in O(1) time
						constructNGram(idx, current_n, sortedTokenList,
								complexTermSet);
					}
				}

			}

		}

		logger.log(Level.INFO, "finished extracting complex units in "
				+ (System.currentTimeMillis() - start) + "ms , "
				+ complexTermSet.size() + " complex terms..");

		logger.log(Level.INFO, "number of ngrams skipped:"
				+ numberNGramsSkipped);

		return complexTermSet;
	}

	// we assemble the ngrams here
	private void constructNGram(int currentIdx, int windowSize,
			List<Annotation> sortedTokenList, Set<ComplexTerm> complexTermSet) {

		for (int w_idx = 1; w_idx <= windowSize; w_idx++) {

			// now that is a bit complicated -- we cannot just create the
			// complex term with the start/end offsets and live happily ever
			// after -- they might change, depending on the satisfaction
			// constraints
			// imposed on leading/trailing ngram components
			// to overcome complicated workflow, it might be better to just
			// rewrite/overwrite start/end values

			// init complex term by ngram-offsets
			ComplexTerm complexTerm = null;
			containsNoun = false;
			try {
				complexTerm = new ComplexTermImpl(sortedTokenList.get(
						currentIdx - windowSize + w_idx).getStartNode()
						.getOffset().longValue(), sortedTokenList.get(
						currentIdx + w_idx - 1).getEndNode().getOffset()
						.longValue());
			} catch (IndexOutOfBoundsException exn) {
				logger.log(Level.FINEST,
						"end of index, no ngram constructed, next please");
				continue;
			}

			// if lemma value not <unknown>, do extract, otherwise use string
			// value
			complexTerm
					.addCueToken(sortedTokenList.get(currentIdx).getFeatures()
							.get(KeywordAnalyser.lemmaFeatureName).equals(
									KeywordAnalyser.unknownLemmaValue) ? (String) sortedTokenList
							.get(currentIdx).getFeatures().get(
									KeywordAnalyser.lemmaFeatureName)
							: (String) sortedTokenList.get(currentIdx)
									.getFeatures().get(
											KeywordAnalyser.lemmaFeatureName));

			// test for constraint satisfaction
			try {

				// fill in respective values subsequently
				for (int shift_idx = 0; shift_idx < windowSize; shift_idx++) {
					Annotation nextTokenA = sortedTokenList.get(currentIdx
							- windowSize + w_idx + shift_idx);

					if (!isAcceptableNGramComponent(nextTokenA.getFeatures())) {
						throw new Exception("violated ngram-element constraint");
					}

					complexTerm.addTokenComponent((String) nextTokenA
							.getFeatures().get(
									ANNIEConstants.TOKEN_STRING_FEATURE_NAME));

					// if lemma value not <unknown>, do extract, otherwise use
					// string value
					complexTerm
							.addLemmaComponent(nextTokenA.getFeatures().get(
									KeywordAnalyser.lemmaFeatureName).equals(
									KeywordAnalyser.unknownLemmaValue) ? (String) nextTokenA
									.getFeatures()
									.get(
											KeywordAnalyser.TOKEN_STRING_FEATURE_NAME)
									: (String) nextTokenA.getFeatures().get(
											KeywordAnalyser.lemmaFeatureName)
							// (String) nextTokenA
							// .getFeatures().get(
							// KeywordAnalyser.lemmaFeatureName)
							);
				}

			} catch (Exception exn) {
				numberNGramsSkipped++;
				logger
						.log(Level.FINEST,
								"at least one of the n-gram elements not acceptable, skipping whole n-gram");
				continue;
			}

			complexTerm.computeStringValue();
			if (isAcceptableBySurface(complexTerm) && containsNoun) {
				complexTermSet.add(complexTerm);
			}

		}
	}

	// filter out noise, garbage, words not containing alpha numeric
	// characters

	private boolean isAcceptableNGramComponent(FeatureMap fmap) {
		// must not be a stop word && must be a word (must not be a non-word,
		// i.e. '-', etc)
		if (fmap.get(KeywordAnalyser.stopwordFeatureName).equals(
				TOKEN_STOPWORD_FEATURE_VALUE_FALSE)
				&& ((String) fmap.get(ANNIEConstants.TOKEN_KIND_FEATURE_NAME))
						.equals(KeywordAnalyser.TOKEN_KIND_FEATURE_VALUE_WORD)
				&& !((String) fmap
						.get(ANNIEConstants.TOKEN_STRING_FEATURE_NAME))
						.endsWith(DASH_STRING)) {
			// n-gram element must either be NOUN
			if (((String) fmap.get(KeywordAnalyser.coarseCategoryFeatureName))
					.equals(COARSE_POS_TAG_SET.NOUN.toString())) {
				containsNoun = true;
				return true;
			} else
			// or VERB
			if (((String) fmap.get(KeywordAnalyser.coarseCategoryFeatureName))
					.equals(COARSE_POS_TAG_SET.VERB.toString())) {
				return true;
			} else
			// // or ADVERB
			// if (((String)
			// fmap.get(KeywordAnalyser.coarseCategoryFeatureName))
			// .equals(COARSE_POS_TAG_SET.ADVERB.toString())) {
			// return true;
			// } else
			// or ADJECTIVE
			if (((String) fmap.get(KeywordAnalyser.coarseCategoryFeatureName))
					.equals(COARSE_POS_TAG_SET.ADJECTIVE.toString())) {
				return true;
			}
		}

		return false;
	}

	/**
	 * @return true, if the complex term should be accepted, false otherwise
	 */
	private boolean isAcceptableBySurface(ComplexTerm ct) {
		if (ct.getStringValue().length() < ACCEPTABLE_KEYPHRASE_MIN_CHARLENGTH) {
			return false;
		}
		if (!ct.getStringValue().matches(
				"^" + EXTENDED_ALPHANUM_REGEXP_STRING + "+$")) {
			return false;
		}

		return true;
	}

}
