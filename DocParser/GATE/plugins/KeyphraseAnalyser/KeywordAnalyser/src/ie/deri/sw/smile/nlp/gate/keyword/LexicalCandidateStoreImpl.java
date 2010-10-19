/*
 * 
 * LexicalCandidateStoreImpl.java, provides keyword/keyphrase extraction as a GATE plugin
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


import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class LexicalCandidateStoreImpl implements LexicalCandidateStore {

	private Set<LexicalCandidate> seedTermRelevanceSet;
	private Set<LexicalCandidate> seedTermFrequencySet;
	
	public LexicalCandidateStoreImpl() {
		this.seedTermRelevanceSet = new LinkedHashSet<LexicalCandidate>();
		this.seedTermFrequencySet = new LinkedHashSet<LexicalCandidate>();
	}

	
	public void setFrequencySet(Set<LexicalCandidate> s) {
		this.seedTermFrequencySet.addAll(s);
	}


	public void setRelevanceSet(Set<LexicalCandidate> s) {
		this.seedTermRelevanceSet.addAll(s);
	}


	public Set<LexicalCandidate> getRelevanceSet() {
		return seedTermRelevanceSet;
	}

	
	public Set<LexicalCandidate> getFrequencySet() {
		return seedTermFrequencySet;
	}

	
	public void print(){
		
		System.out.println("LEXICAL TERMS WITH COMPUTED RELEVANCE VALUE:");
		Iterator<LexicalCandidate> relevanceItr = seedTermRelevanceSet.iterator();
		while (relevanceItr.hasNext()){
			relevanceItr.next().print();
		}
		relevanceItr = null;
		
		System.out.println("LEXICAL TERMS WITH HIGH FREQUENCY VALUE:");
		Iterator<LexicalCandidate> frequencyItr = seedTermFrequencySet.iterator();
		while (frequencyItr.hasNext()){
			frequencyItr.next().print();
		}
		frequencyItr = null;
	}
	
}
