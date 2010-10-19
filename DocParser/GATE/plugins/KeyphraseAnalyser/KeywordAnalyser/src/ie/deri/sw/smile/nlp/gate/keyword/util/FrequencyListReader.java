/*
 * 
 * FrequencyListReader.java, provides keyword/keyphrase extraction as a GATE plugin
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

package ie.deri.sw.smile.nlp.gate.keyword.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

public class FrequencyListReader extends BufferedReader {

	private int corpus_size = -1;
	private int lexicon_size = -1;

	private static final Pattern RANK_FREQ_TOKEN_PATTERN = Pattern
			.compile("^\\d+\\s\\d+.\\d\\d\\s\\w+$");
	private static final Pattern NUMBER_PATTERN = Pattern
			.compile("\\D(\\d+)\\D");

	private NormalisedFrequencyBearer currentFrequencyBearer = null;

	private static Logger logger = Logger.getLogger(FrequencyListReader.class.getName());

	public FrequencyListReader(Reader reader) {
		super(reader);

		readCorpusProperties();
	}

	public NormalisedFrequencyBearer readFrequencyLine() throws IOException {
		currentFrequencyBearer = null;

		String[] rankFreqToken = super.readLine().split(" ");
		try {
			return new NormalisedFrequencyBearerImpl(Integer
					.parseInt(rankFreqToken[0]), rankFreqToken[2], Float
					.valueOf(rankFreqToken[1]).floatValue(), this
					.computeRealFrequency(Float.valueOf(rankFreqToken[1])
							.floatValue()));
		} catch (NumberFormatException exn) {
			logger.log(Level.INFO, "did not insert <" + rankFreqToken[2]
					+ "> : " + exn.getMessage());
			logger.log(Level.INFO, "skipping this line, trying next line" );
		} finally {
			rankFreqToken = null;
		}
		
		return readFrequencyLine();
	}

	public int getCorpusSize() {
		return corpus_size;
	}

	public int getLexiconSize() {
		return lexicon_size;
	}

	// read corpus properties and set values for corpus size and lexicon size
	// which is necessary for computing total frequencies from the respective
	// list
	private void readCorpusProperties() {

		try {
			String line = null;

			do {
				// mark present position so that we dont have to
				// read it again for the actual frequencies
				this.mark(1000); // the sought values are in the upper 4
				// lines,
				// so 1000 chars should be sufficiently conservative
				line = this.readLine();
				Matcher numberMatcher = NUMBER_PATTERN.matcher(line);
				if (line.contains("corpus size") && numberMatcher.find()) {
					corpus_size = Integer.parseInt(numberMatcher.group(1));
				}
				if (line.contains("lexicon size") && numberMatcher.find()) {
					lexicon_size = Integer.parseInt(numberMatcher.group(1));
				}
				numberMatcher = null;
			} while (!RANK_FREQ_TOKEN_PATTERN.matcher(line).matches());

			line = null;
			this.reset(); // reset to first occurrence of frequency token
			// pattern
		} catch (IOException exn) {
			logger.log(Level.WARNING,
					"could not read corpus properties, check input file", exn);
		}
	}

	// calcualtes the real frequency for a token from the frequency lists, which
	// hold
	// normalized frequencies (occurrences per million) instead
	private int computeRealFrequency(float normalizedFrequency) {
		return Math
				.round(normalizedFrequency * ((float) corpus_size / 1000000));
	}

	class NormalisedFrequencyBearerImpl implements NormalisedFrequencyBearer {

		private int rank = -1;
		private String token = null;
		private float normalised_frequency = 0f;
		private int real_frequency = -1;

		public NormalisedFrequencyBearerImpl(int rank, String token,
				float normalizedFrequency, int realFrequency) {
			this.rank = rank;
			this.token = token;
			this.normalised_frequency = normalizedFrequency;
			this.real_frequency = realFrequency;
		}

		public int getRank() {
			return rank;
		}

		public String getToken() {
			return token;
		}

		public float getNormalisedFrequency() {
			return normalised_frequency;
		}

		public int getRealFrequency() {
			return real_frequency;
		}
	}

}
