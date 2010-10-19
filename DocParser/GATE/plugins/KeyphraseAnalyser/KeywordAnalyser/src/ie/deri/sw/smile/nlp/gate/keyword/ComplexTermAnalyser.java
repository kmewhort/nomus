/*
 * 
 * ComplexTermAnalyser.java, provides keyword/keyphrase extraction as a GATE plugin
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.wcohen.ss.AbstractStringDistance;
import com.wcohen.ss.Level2MongeElkan;

public class ComplexTermAnalyser {

	private static Logger logger = Logger.getLogger(ComplexTermAnalyser.class
			.getName());

	// private static final AbstractStringMetric STRINGMETRIC = new
	// MongeElkan();
	private static final AbstractStringDistance STRINGMETRIC = new Level2MongeElkan();
	private static final double SIMILARITY_THRESHOLD = 0.80;

	private static final double CTS_WEIGHT = 0.3; // used to be 0.25
	private static final double NOW_WEIGHT = 0.2;
	private static final double SOC_WEIGHT = 0.5;

	// we dont use it anymore - score moved into significance as significant cue
	// token scores are accumulated as well
	private static final double CTO_WEIGHT = 0.0; // used to be 0.25, now
	private static final double CTS_SCALE = 10.0;

	private static final double CONFIDENCE_THRESHOLD = 0.1;
	private static final double FREQUENCY_THRESHOLD = 1;
	private static final double SIGNIFICANCE_THRESHOLD = 3.84;

	private long docStart = -1;
	private long docEnd = -1;
	private int docTokenLength = -1;

	public ComplexTermAnalyser(gate.Document document) {
		docStart = document.getAnnotations().firstNode().getOffset()
				.longValue();
		docEnd = document.getAnnotations().lastNode().getOffset().longValue();
		docTokenLength = ((Integer) document.getFeatures().get(
				KeywordAnalyser.documentSizeFeatureName)).intValue();
	}

	/**
	 * 
	 * @param complexTermSet
	 * @return
	 */
	public SortedSet<KeywordOrPhraseCandidate> analyse(
			Set<ComplexTerm> complexTermSet) {
		// initalize sortedSet with appropriate comparator
		SortedSet<KeywordOrPhraseCandidate> kwcSet = new TreeSet<KeywordOrPhraseCandidate>(
				new Comparator<KeywordOrPhraseCandidate>() {
					public int compare(KeywordOrPhraseCandidate o1,
							KeywordOrPhraseCandidate o2) {
						if (o1.getConfidence() > o2.getConfidence()) {
							return -1;
						} else if (o1.getConfidence() < o2.getConfidence()) {
							return 1;
						} else {
							return o2.getCueTokens().length
									- o1.getCueTokens().length;
						}
					}
				});

		PositionAnalyser positionAnalyser = new PositionAnalyser(docStart,
				docEnd);
		// now, the outcome here is a set of sorted sets
		Set<SortedSet<ComplexTerm>> partitionedComplexTermSet = partition(complexTermSet);
		long start = System.currentTimeMillis();
		Iterator<SortedSet<ComplexTerm>> ctSetItr = partitionedComplexTermSet
				.iterator();
		while (ctSetItr.hasNext()) {
			// this is a sorted set
			SortedSet<ComplexTerm> cluster = ctSetItr.next();
			KeywordOrPhraseCandidate kwc = new KeywordOrPhraseCandidateImpl(
					clusterAnalysis(cluster));
			fold(cluster, kwc);
			kwc.setClusterStrings(cluster);
			positionAnalyser.analyse(kwc);
			computeConfidence(kwc);

			if (kwc.getConfidence() > CONFIDENCE_THRESHOLD
					&& kwc.getFrequency() > FREQUENCY_THRESHOLD
					// if document is of sufficiently large size, we enforce
					// scope restriction,
					&& (docTokenLength > 1000 ? (kwc.hasGlobalScope() || kwc
							.hasLocalScope())
					// if document very small, we take all we can get
							: true)) {
				kwcSet.add(kwc);
			}
		}

		logger.log(Level.INFO,
				"finshed analysing partitioned complex term clusters. Number of candidates: "
						+ kwcSet.size() + ". Elapsed time: "
						+ (System.currentTimeMillis() - start) + "ms");
		return kwcSet;
	}

	private static final String WHITESPACE = " ";

	private void computeConfidence(KeywordOrPhraseCandidate kwc) {

		double frequency = (double) kwc.getFrequency();
		double significance = kwc.getSignificance();

		double significanceContribution = CTS_WEIGHT
				- (1.0 / (1.0 + Math.log1p(significance > 0.0 ? significance
						: frequency * CTS_SCALE)));
		double wordnumberContribution = NOW_WEIGHT
				- (NOW_WEIGHT / (double) (kwc.getStringValue()
						.split(WHITESPACE).length == 0 ? 1 : kwc
						.getStringValue().split(WHITESPACE).length));
		double cuetokenContribution = CTO_WEIGHT
				* (kwc.getCueTokens().length == 0.0 ? 0.0
						: kwc.getCueTokens().length == 1 ? 0.5 : 1.0);
		double scopeContribution = SOC_WEIGHT
				* (kwc.hasGlobalScope() ? 1.0 : kwc.hasLocalScope() ? 0.5 : 0.0);

		kwc
				.setConfidence((double) (significanceContribution
						+ wordnumberContribution + cuetokenContribution + scopeContribution));
	}

	/**
	 * 
	 * @param complexTermCluster
	 * @return
	 */
	private String clusterAnalysis(SortedSet<ComplexTerm> complexTermCluster) {

		String kwcStringValue = maximiseSimilarity(STRINGMETRIC,
				complexTermCluster);

		return kwcStringValue;
	}

	/**
	 * @param complexTermCluster
	 * @param kwc
	 */
	private void fold(Set<ComplexTerm> complexTermCluster,
			KeywordOrPhraseCandidate kwc) {

		long[] offsets = new long[complexTermCluster.size()];
		int offsetIdx = 0;

		Set<String> cueTokenSet = new HashSet<String>();

		// System.out.println("KEYWORD : "+ kwc.getStringValue());
		// StringBuffer buf = new StringBuffer();
		// buf.append(" ");

		// we want to assign the maximum significance found in the whole cluster
		// to the kwc
		double maxSignificance = 0.0; // in order to compute this, we also
		// multiply by the number of words over
		// which is being compared

		Iterator<ComplexTerm> ctItr = complexTermCluster.iterator();
		while (ctItr.hasNext()) {
			ComplexTerm ct = ctItr.next();
			offsets[offsetIdx] = ct.getStartOffset();
			for (String s : ct.getCueTokens()) {
				cueTokenSet.add(s);
			}

			if (ct.getCueTokenSignificanceValue() > maxSignificance) {
				maxSignificance = ct.getCueTokenSignificanceValue();
			}

			// buf.append(ct.getStringValue());
			// if ( ctItr.hasNext()){
			// buf.append(" , ");
			// }

			offsetIdx++;
		}

		// System.out.println(buf.toString());
		// buf = null;

		kwc.setSignificance(maxSignificance);
		kwc.setOffsetDistribution(offsets);
		kwc.setFrequency(offsets.length);
		kwc.setCueTokens(cueTokenSet.toArray(new String[cueTokenSet.size()]));

		// clean up
		offsets = null;
		cueTokenSet.clear();
		cueTokenSet = null;
	}

	/**
	 * find the string which maximises string similarity among the comparisons
	 * in his cluster
	 * <p>
	 * invariant: the cluster is already sorted by the token length of elements,
	 * descending order - this helps with computing similarity, and assign
	 * longer terms higher priority <br>
	 * <p>
	 * 2. maximise similarity , similarity among longer matches should have
	 * higher score than among shorter matches
	 * <p>
	 * 
	 * @param stringmetric
	 * @param cluster
	 * @return the string
	 */
	private String maximiseSimilarity(AbstractStringDistance stringmetric,
			SortedSet<ComplexTerm> cluster) {

		double max = 0.0;
		ComplexTerm maxComplexTerm = null;

		// these structures hold maximised accumulated similarity and
		// observations of how many times a similarity was recorded to be above
		// similarity threshold
		double[] scoreArray = new double[cluster.size()];

		// we need a list to allow access by index
		List<ComplexTerm> clusterList = new ArrayList(cluster);

		// well, this one is quadratic..
		for (int ctIdx = 0; ctIdx < clusterList.size(); ctIdx++) {
			ComplexTerm complexTerm = clusterList.get(ctIdx);

			double similaritySum = 0.0;
			int obsSimAboveThreshold = 0;

			for (int ctJdx = 0; ctJdx < clusterList.size(); ctJdx++) {

				ComplexTerm otherComplexTerm = clusterList.get(ctJdx);
				double sim = stringmetric.score(complexTerm.getStringValue(),
						otherComplexTerm.getStringValue());

				similaritySum += sim;

				if (sim >= SIMILARITY_THRESHOLD) {
					obsSimAboveThreshold++;
				}
			}

			// the boosting function
//			double boost = complexTerm.getTokenComponents().length - (complexTerm
//					.getTokenComponents().length / obsSimAboveThreshold);

			// the new boosting function
			double boost = Math.log( complexTerm.getTokenComponents().length *  obsSimAboveThreshold ) ;

			scoreArray[ctIdx] =	boost * similaritySum;

		}

		// we need to find the maximum complexTerm here in an extra loop over
		// those arrays
		double maximum = -1.0;
		for (int idx = 0; idx < scoreArray.length; idx++) {

			if (scoreArray[idx] > maximum) {
				maximum = scoreArray[idx];
				maxComplexTerm = clusterList.get(idx);
			}
		}

		return maxComplexTerm.getStringValue();
	}

	/**
	 * 
	 * @param complexTermSet
	 * @return
	 */
	private Set<SortedSet<ComplexTerm>> partition(
			Set<ComplexTerm> complexTermSet) {

		Set<SortedSet<ComplexTerm>> clusterSet = new HashSet<SortedSet<ComplexTerm>>();
		int comparisons = 0;
		long start = System.currentTimeMillis();

		Iterator<ComplexTerm> ctItr = complexTermSet.iterator();
		while (ctItr.hasNext()) {
			SortedSet<ComplexTerm> cluster = null;
			ComplexTerm complexTerm = ctItr.next();

			// we need a reference for comparison with current element of set
			// initially, we start with the first one we can grab
			if (!complexTerm.isMarked()) {
				complexTerm.mark();
				// the original approach
				// cluster = new HashSet<ComplexTerm>();
				//
				// the alternative approach:
				// we sort the cluster elements here already, by descending
				// number of words,
				// which will be needed later on in fold, as we would like to
				// compare elements
				// with longest elements first, as this gives more weight to
				// similarity among long elements
				cluster = new TreeSet<ComplexTerm>(
						new Comparator<ComplexTerm>() {

							public int compare(ComplexTerm ct1, ComplexTerm ct2) {
								if (ct1.getTokenComponents().length > ct2
										.getTokenComponents().length) {
									return -1;
								} else {
									return 1;
								}
							}
						});
				cluster.add(complexTerm);

				// looks quadratic, but is not because we skip elements that
				// have been dealt with, so is more like n * log n
				Iterator<ComplexTerm> innerComplexTermItr = complexTermSet
						.iterator();
				while (innerComplexTermItr.hasNext()) {
					ComplexTerm otherComplexTerm = innerComplexTermItr.next();
					// skip already marked / allocated entries, each initial
					// value is unmarked
					if (!otherComplexTerm.isMarked()) {
						comparisons++;
						if (complexTerm.isStrikinglySimilar(STRINGMETRIC,
								otherComplexTerm, SIMILARITY_THRESHOLD)) {
							cluster.add(otherComplexTerm);
							otherComplexTerm.mark();
						}
					}
				}
				clusterSet.add(cluster);
			}
		}
		logger.log(Level.INFO, "COMPARISONS:" + comparisons + " -- CLUSTERS: "
				+ clusterSet.size() + " -- ELAPSED TIME:"
				+ (System.currentTimeMillis() - start) + "ms");

		return clusterSet;
	}

}
