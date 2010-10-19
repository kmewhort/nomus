/*
 * 
 * ChunkStrategy.java, provides keyword/keyphrase extraction as a GATE plugin
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

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ChunkStrategy implements ComplexUnitStrategy {

	private static Logger logger = Logger.getLogger(ChunkStrategy.class
			.getName());

	// we should delete these after some performance analysis
	private AnnotationSet tokenAnnotations;
	private AnnotationSet chunkAnnotations;

	private static long iterations = 0;

	private SortedMap<Long, Annotation> tokenAnnotationMap;
	private SortedMap<Long, Annotation> chunkAnnotationMap;

	static int numberChunksSkipped = 0;

	public ChunkStrategy(AnnotationSet annotations) {
		this.tokenAnnotations = annotations
				.get(ANNIEConstants.TOKEN_ANNOTATION_TYPE);
		this.chunkAnnotations = annotations
				.get(KeywordAnalyser.nounchunkAnnotationType);

		this.tokenAnnotationMap = new TreeMap<Long, Annotation>();
		this.chunkAnnotationMap = new TreeMap<Long, Annotation>();

		transformAnnotationSet2Map(tokenAnnotations, tokenAnnotationMap);
		transformAnnotationSet2Map(chunkAnnotations, chunkAnnotationMap);

	}

	// we can probably make-do with only referencing the iterator, if that works
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
		numberChunksSkipped = 0;

		// this is where we collect the complex terms, which are analysed later
		// for KeywordOrPhraseCandidates
		Set<ComplexTerm> complexTermSet = new LinkedHashSet<ComplexTerm>();

		long start = System.currentTimeMillis();

		Iterator<LexicalCandidate> relevanceItr = lexicalCandidateStore
				.getRelevanceSet().iterator();
		// foreach relevant lexical item determined by statistical calculation
		while (relevanceItr.hasNext()) {
			LexicalCandidate lexCand = relevanceItr.next();

			extractComplexTerm(lexCand, complexTermSet);
		}

		Iterator<LexicalCandidate> frequencyItr = lexicalCandidateStore
				.getFrequencySet().iterator();
		// foreach high frequency lexical item
		while (frequencyItr.hasNext()) {
			LexicalCandidate lexCand = frequencyItr.next();

			extractComplexTerm(lexCand, complexTermSet);
		}

		logger.log(Level.INFO, "finished extracting complex units in "
				+ (System.currentTimeMillis() - start) + "ms , "
				+ complexTermSet.size() + " complex terms..");

		logger.log(Level.INFO, "number of chunks skipped:"
				+ numberChunksSkipped);

		return complexTermSet;
	}

	private boolean matchesLexicalCandidate(Annotation tokenA,
			LexicalCandidate lexCand) {
		FeatureMap fmap = tokenA.getFeatures();
		// in case we have a lemma of type "<unknown>"
		if (fmap.get(KeywordAnalyser.lemmaFeatureName).equals(
				KeywordAnalyser.unknownLemmaValue)) {

			if (fmap.get(KeywordAnalyser.TOKEN_STRING_FEATURE_NAME).equals(
					lexCand.getStringValue())
					&& fmap.containsKey(KeywordAnalyser.stopwordFeatureName)
					&& fmap.get(KeywordAnalyser.stopwordFeatureName).equals(
							TOKEN_STOPWORD_FEATURE_VALUE_FALSE)) {
				return true;
			}

		} else {

			if (fmap.get(KeywordAnalyser.lemmaFeatureName).equals(
					lexCand.getStringValue())
					&& fmap.containsKey(KeywordAnalyser.stopwordFeatureName)
					&& fmap.get(KeywordAnalyser.stopwordFeatureName).equals(
							TOKEN_STOPWORD_FEATURE_VALUE_FALSE)) {
				return true;
			}
		}

		return false;
	}

	private void extractComplexTerm(LexicalCandidate lexCand,
			Set<ComplexTerm> complexTermSet) {
		Iterator<Annotation> tokAnnotItr = tokenAnnotationMap.values()
				.iterator();
		// find token annotations matching lexical candidate
		while (tokAnnotItr.hasNext()) {
			Annotation tokAnnot = tokAnnotItr.next();
			FeatureMap fmap = tokAnnot.getFeatures();
			// extract values only if we can establish a match
			if (matchesLexicalCandidate(tokAnnot, lexCand)) {
				Long tokenStart = tokAnnot.getStartNode().getOffset();

				/* ## A ## */
				Long chunkStart;
				Long chunkEnd;
				if (chunkAnnotationMap.containsKey(tokenStart)) {
					chunkStart = chunkAnnotationMap.get(tokenStart)
							.getStartNode().getOffset();
					chunkEnd = chunkAnnotationMap.get(tokenStart).getEndNode()
							.getOffset();
				} else if (chunkAnnotationMap.firstKey().longValue() < tokenStart
						.longValue()) {
					chunkStart = chunkAnnotationMap.get(
							chunkAnnotationMap.headMap(tokenStart).lastKey())
							.getStartNode().getOffset();
					chunkEnd = chunkAnnotationMap.get(
							chunkAnnotationMap.headMap(tokenStart).lastKey())
							.getEndNode().getOffset();
				} else {
					continue;
				}

				// we initialize a complex term here and fill in details
				// subsequently
				ComplexTerm complexTerm = new ComplexTermImpl(chunkStart
						.longValue(), chunkEnd.longValue());
				complexTerm.addCueToken(lexCand.getStringValue());
				// we assign frequency in case relevance for lexCand has not
				// been initialized (still on 0.0 default)
				// assignment of actual relevance otherwise (in case it has been
				// initalized/set)
				complexTerm
						.addCueTokenSignificanceValue(lexCand.getRelevance() == 0.0 ? lexCand
								.getFrequency()
								: lexCand.getRelevance());
				boolean isInitial = true;

				/* ## B ## */
				// we have to add +1 to chunkEnd as submap excludes the
				// end boundary, but we need to include it
				Iterator<Annotation> chunkTokenItr = tokenAnnotationMap.subMap(
						chunkStart, Long.valueOf(chunkEnd.longValue() + 1))
						.values().iterator();
				while (chunkTokenItr.hasNext()) {
					Annotation chunkTokenA = chunkTokenItr.next();
					// we discard initial stopwords of a phrase here
					if (isInitial) {
						if (!filterInitialToken(chunkTokenA.getFeatures())) {
							// fill in token details
							repairToken(chunkTokenItr, chunkTokenA
									.getFeatures(), complexTerm);
							isInitial = false;
						}
					} else if (!chunkTokenItr.hasNext()) { // is final
						if (!filterFinalToken(chunkTokenA.getFeatures())) {
							repairToken(chunkTokenItr, chunkTokenA
									.getFeatures(), complexTerm);
						}
					} else {
						repairToken(chunkTokenItr, chunkTokenA.getFeatures(),
								complexTerm);
					}
				}

				complexTerm.computeStringValue();
				if (!isUnacceptableBySurface(complexTerm)) {
					complexTermSet.add(complexTerm);
				} else {
					numberChunksSkipped++;
				}
			}

		}
	}

	// glues together tokens (AND LEMMAS!) that have been separated by
	// hyphenation,etc..
	// adds the repaired stuff to complexTerm
	private void repairToken(Iterator<Annotation> tokenItr,
			FeatureMap tokenFeatures, ComplexTerm complexTerm) {
		String repairedTokenStr = (String) tokenFeatures
				.get(ANNIEConstants.TOKEN_STRING_FEATURE_NAME);
		String repairedLemmaStr = tokenFeatures.get(
				KeywordAnalyser.lemmaFeatureName).equals(
				KeywordAnalyser.unknownLemmaValue) ? (String) tokenFeatures
				.get(KeywordAnalyser.TOKEN_STRING_FEATURE_NAME)
				: (String) tokenFeatures.get(KeywordAnalyser.lemmaFeatureName);
		if (repairedTokenStr.endsWith(DASH_STRING) && tokenItr.hasNext()) {
			Annotation tokenAnnotation = tokenItr.next();

			repairedTokenStr = repairedTokenStr.substring(0,
					repairedTokenStr.lastIndexOf(DASH_STRING)).concat(
					(String) tokenAnnotation.getFeatures().get(
							ANNIEConstants.TOKEN_STRING_FEATURE_NAME));
			repairedLemmaStr = repairedLemmaStr
					.substring(0, repairedLemmaStr.lastIndexOf(DASH_STRING))
					.concat(
							tokenFeatures.get(KeywordAnalyser.lemmaFeatureName)
									.equals(KeywordAnalyser.unknownLemmaValue) ? (String) tokenFeatures
									.get(KeywordAnalyser.TOKEN_STRING_FEATURE_NAME)
									: (String) tokenFeatures
											.get(KeywordAnalyser.lemmaFeatureName));
		}
		complexTerm.addTokenComponent(repairedTokenStr);
		complexTerm.addLemmaComponent(repairedLemmaStr);

		// logger.log(Level.FINEST, "REPAIRED HYPHEN: "+ repairedTokenStr);
	}

	// filter out noise, garbage, words not containing alpha numeric
	// characters, chunks with more than 5 words
	/**
	 * @return true, if the complex term should be rejected, false otherwise
	 */
	private boolean isUnacceptableBySurface(ComplexTerm ct) {
		boolean retval = false;

		if (ct.getStringValue().length() < ACCEPTABLE_KEYPHRASE_MIN_CHARLENGTH) {
			retval = true;
		}
		if (!ct.getStringValue().matches(
				"^" + EXTENDED_ALPHANUM_REGEXP_STRING + "+$")) {
			retval = true;
		}
		if (ct.getStringValue().split(" ").length > ACCEPTABLE_KEYPHRASE_MAX_TOKEN_SIZE) {
			retval = true;
		}

		return retval;
	}

	private boolean filterInitialToken(FeatureMap tokenFeatures) {

		boolean retval = false;
		
		if (tokenFeatures.containsKey(KeywordAnalyser.stopwordFeatureName)
				&& tokenFeatures.get(KeywordAnalyser.stopwordFeatureName)
						.equals(TOKEN_STOPWORD_FEATURE_VALUE_TRUE)) {
			retval = true;
		}
		if (tokenFeatures.get(ANNIEConstants.TOKEN_KIND_FEATURE_NAME).equals(
				TOKEN_KIND_FEATURE_VALUE_NUMBER)) {
			retval = true;
		}
		if (tokenFeatures
				.containsKey(KeywordAnalyser.coarseCategoryFeatureName)
				&& tokenFeatures.get(KeywordAnalyser.coarseCategoryFeatureName)
						.equals(
								COARSE_POS_TAG_SET.DETERMINER_OR_PRONOUN
										.toString())) {
			retval = true;
		}
		if (!tokenFeatures.get(ANNIEConstants.TOKEN_KIND_FEATURE_NAME).equals(
				KeywordAnalyser.TOKEN_KIND_FEATURE_VALUE_WORD)) {
			retval = true;
		}

		return retval;
	}

	private boolean filterFinalToken(FeatureMap tokenFeatures) {
 
		boolean retval = false;
		
		if (!tokenFeatures.get(ANNIEConstants.TOKEN_KIND_FEATURE_NAME).equals(
				KeywordAnalyser.TOKEN_KIND_FEATURE_VALUE_WORD)) {
			retval = true;
		}
		// filter last token if it is stopword
		if (tokenFeatures.containsKey(KeywordAnalyser.stopwordFeatureName) && tokenFeatures.get(KeywordAnalyser.stopwordFeatureName).equals(
				TOKEN_STOPWORD_FEATURE_VALUE_TRUE)) {
			retval = true;
		}
		// filter last token if it is DET or PRON
		if ( tokenFeatures.containsKey(KeywordAnalyser.coarseCategoryFeatureName) && tokenFeatures.get(KeywordAnalyser.coarseCategoryFeatureName)
				.equals(COARSE_POS_TAG_SET.DETERMINER_OR_PRONOUN.toString())) {
			retval = true;
		}
		// filter last token if it is ADVERB
		if ( tokenFeatures.containsKey(KeywordAnalyser.coarseCategoryFeatureName) && tokenFeatures.get(KeywordAnalyser.coarseCategoryFeatureName)
				.equals(COARSE_POS_TAG_SET.ADVERB.toString())) {
			retval = true;
		}

		return retval;
	}

}
