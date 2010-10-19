/*
 * 
 * FrequencyBearer.java, provides frequency analysis as a GATE plugin
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

import java.util.Iterator;
import java.util.Map;

class FrequencyBearer {

	Map<String, Integer> tokenFreqM;
	Map<String, Integer> rootFreqM;
	Map<String, Integer> nounlemmaFreqM;
	Map<String, Integer> verblemmaFreqM;
	Map<String, Integer> adjlemmaFreqM;
	Map<String, Integer> posFreqM;

	public Map<String, Integer> getTokenFreqM() {
		return tokenFreqM;
	}

	public void setTokenFreqM(Map<String, Integer> tokenFreqM) {
		this.tokenFreqM = tokenFreqM;
	}

	public Map<String, Integer> getRootFreqM() {
		return rootFreqM;
	}

	public void setRootFreqM(Map<String, Integer> rootFreqM) {
		this.rootFreqM = rootFreqM;
	}

	public Map<String, Integer> getNounlemmaFreqM() {
		return nounlemmaFreqM;
	}

	public void setNounlemmaFreqM(Map<String, Integer> nounlemmaFreqM) {
		this.nounlemmaFreqM = nounlemmaFreqM;
	}

	public Map<String, Integer> getVerblemmaFreqM() {
		return verblemmaFreqM;
	}

	public void setVerblemmaFreqM(Map<String, Integer> verblemmaFreqM) {
		this.verblemmaFreqM = verblemmaFreqM;
	}

	public Map<String, Integer> getAdjlemmaFreqM() {
		return adjlemmaFreqM;
	}

	public void setAdjlemmaFreqM(Map<String, Integer> adjlemmaFreqM) {
		this.adjlemmaFreqM = adjlemmaFreqM;
	}

	public Map<String, Integer> getPosFreqM() {
		return posFreqM;
	}

	public void setPosFreqM(Map<String, Integer> posFreqM) {
		this.posFreqM = posFreqM;
	}

	public int getDocumentSize() {
		int docsize = 0;
		Iterator<Map.Entry<String, Integer>> tokenItr = tokenFreqM
				.entrySet().iterator();
		while (tokenItr.hasNext()) {
			docsize += tokenItr.next().getValue().intValue();
		}
		return docsize;
	}

	public int getLexiconSize() {
		return rootFreqM.size();
	}

	public void clear() {
		tokenFreqM.clear();
		tokenFreqM = null;
		rootFreqM.clear();
		rootFreqM = null;
		nounlemmaFreqM.clear();
		nounlemmaFreqM = null;
		verblemmaFreqM.clear();
		verblemmaFreqM = null;
		adjlemmaFreqM.clear();
		adjlemmaFreqM = null;
		posFreqM.clear();
		posFreqM = null;
	}
}
