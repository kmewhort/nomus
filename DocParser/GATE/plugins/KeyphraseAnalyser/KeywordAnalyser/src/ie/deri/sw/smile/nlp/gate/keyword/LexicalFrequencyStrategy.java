/*
 * 
 * LexicalFrequencyStrategy.java, provides keyword/keyphrase extraction as a GATE plugin
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

import ie.deri.sw.smile.nlp.gate.keyword.util.LexicalCandidateComparator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

public class LexicalFrequencyStrategy implements LexicalStrategy {

	private static Logger logger = Logger.getLogger(LexicalFrequencyStrategy.class.getName());
	
	private Set<Map.Entry<String, Integer>> seedTokenSet;

	public LexicalFrequencyStrategy(Set<Map.Entry<String, Integer>> seedTokenSet) {
		this.seedTokenSet = seedTokenSet;
	}

	public LexicalCandidateStore produceCandidateTerms(int refCorpusSize,
			Map<String, Integer> refFrequencyMap) {

		long start = System.currentTimeMillis();
		
		List<LexicalCandidate> frequencyList = new ArrayList<LexicalCandidate>();

		Iterator<Map.Entry<String, Integer>> eItr = seedTokenSet.iterator();
		while (eItr.hasNext()) {
			Map.Entry<String, Integer> entry = eItr.next();

			// only add seedToken if it has not been classified as a stopword
			
			frequencyList.add(new LexicalCandidateImpl(entry.getKey(), entry
					.getValue()));
		}

		LexicalCandidateStore lexicalCandidateStore = new LexicalCandidateStoreImpl();

		Collections.sort(frequencyList, new LexicalCandidateComparator());

		lexicalCandidateStore
				.setFrequencySet(new LinkedHashSet<LexicalCandidate>(
						frequencyList));

		// clean up
		refFrequencyMap.clear();
		refFrequencyMap = null;
		seedTokenSet.clear();
		seedTokenSet = null;
		frequencyList.clear();
		frequencyList = null;

		logger.log(Level.INFO, "finished computing lexical relevance statistics in "+ (System.currentTimeMillis()-start)+"ms ..");

		return lexicalCandidateStore;
	}

}
