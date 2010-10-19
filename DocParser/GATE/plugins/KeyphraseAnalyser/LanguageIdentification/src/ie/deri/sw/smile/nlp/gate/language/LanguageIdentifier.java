/*
 * 
 * LanguageIdentifier.java, provides language identification as a GATE plugin
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


package ie.deri.sw.smile.nlp.gate.language;

import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;

import java.io.IOException;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;

import de.spieleck.app.cngram.NGramProfiles;

/**
 * This class is the implementation of the resource LANGUAGEIDENTIFICATION.
 * tries to identify the language of a given document
 * <p>
 * can be plugged into a serial analyser controller
 * <p>
 * enriches the global document description with the language feature
 * <p>
 * makes use of Frank S. Nestel's ngram4j library: http://ngramj.sourceforge.net
 * (LGPL)
 * <p>
 * to avoid long waiting times partitions the input text into smaller (but
 * reasonably long) fractions and randomly selects a subset of those to perform
 * language analysis
 */
public class LanguageIdentifier extends AbstractLanguageAnalyser {

	private static final long serialVersionUID = -8074602831406702662L;

	private static Logger logger = Logger.getLogger(LanguageIdentifier.class
			.getName());

	private NGramProfiles nps = null;

	public LanguageIdentifier() {

	}

	@Override
	public Resource init() throws ResourceInstantiationException {

		try {
			nps = new NGramProfiles();
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < nps.getProfileCount(); i++) {
				sb.append(nps.getProfileName(i));
				if (i < nps.getProfileCount() - 1) {
					sb.append(",");
				}
			}
			logger.log(Level.INFO, "profiles loaded for languages: "
					+ sb.toString());
		} catch (IOException exn) {
			logger.log(Level.WARNING, "exception during init", exn);
		}

		return this;
	}

	@Override
	public void execute() throws ExecutionException {

		String language = null;

		try {

			String text = document.getContent().toString();

			language = identifyLanguage(text);
			// language = "en";

			if (language != null) {
				document.getFeatures().put(languageFeatureName, language);
				logger.log(Level.FINE, "detected language for "
						+ document.getName() + " : "
						+ document.getFeatures().get(languageFeatureName));
			} else {
				throw new RuntimeException(
						"language detection failed, no language assigned..");
			}

		} catch (Exception exn) {
			throw new ExecutionException("execption in "
					+ LanguageIdentifier.class.getName(), exn);
		}

	}

	/**
	 * identifies the language of an input text
	 * 
	 * @param text
	 *            the input text as string
	 * @return the language string as 2-letter code ( ISO639 / ISO3166-1 ) TODO:
	 *         need to check which one precisely
	 */
	private String identifyLanguage(String text) throws RuntimeException {

		NGramProfiles.Ranker ranker = nps.getRanker();

		// ranker.account(text);
		ranker.account(partitionText(text));

		NGramProfiles.RankResult res = ranker.getRankResult();

		String language = null;

		int idx = getBestResultIdx(res);

		language = res.getName(idx);

		// if confidence is too low, we don't do anything
		if (res.getScore(idx) < 0.3) {
                            logger.log(Level.WARNING, "low confidence for language identification,  assigning default language: en");
                            language = "en";
//			throw new RuntimeException(
//					"low confidence for language identification, no language assigned:"
//							+ res.getName(idx) + " - " + res.getScore(idx));
		}

		// some cleaning up
		res = null;
		ranker = null;

		return language;
	}

	// partitions text into smaller fractions (of maxPartitionChars chars each)
	private String partitionText(String text) {

		int length = document.getContent().toString().length();
		int tenPercent = length / 10;

		// text too short to do this kind of partitioning, thus it will be fast anyway 
		if (length - 2 * tenPercent < maxPartitionChars) {
			return text;
		} else {

			Random rand = new Random();

			// get start offset somewhere beyond first 10% of the document,
			// at least 1000 chars before last 10% of document
			int startOffset = rand.nextInt(length - 2 * tenPercent
					- maxPartitionChars)
					+ tenPercent;
			int endOffset = startOffset + maxPartitionChars;
			logger.log(Level.FINE, "startOffset: " + startOffset
					+ " -- endOffset: " + endOffset);
			return text.substring(startOffset, endOffset);
		}
	}

	// gets the best result from a bunch of ranked results,
	// we better do this as we have no guarantee that the result is ordered
	private int getBestResultIdx(NGramProfiles.RankResult res) {

		double max = 0.0d;
		int maxIdx = -1;

		for (int i = 0; i < res.getLength(); i++) {
			double current = res.getScore(i);
			if (current > max) {
				max = current;
				maxIdx = i;
			}
		}

		return maxIdx;
	}

	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

	public String getEncoding() {
		return this.encoding;
	}

	public void setLanguageFeatureName(String languageFeatureName) {
		this.languageFeatureName = languageFeatureName;
	}

	public String getLanguageFeatureName() {
		return this.languageFeatureName;
	}

	public void setMaxPartitionChars(Integer max) {
		this.maxPartitionChars = max;
	}

	public Integer getMaxPartitionChars() {
		return this.maxPartitionChars;
	}

	private String encoding;
	private String languageFeatureName;
	private Integer maxPartitionChars;

} // class LanguageIdentifier
