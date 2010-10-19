/*
 * 
 * LexicalCandidateImpl.java, provides keyword/keyphrase extraction as a GATE plugin
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


import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public class LexicalCandidateImpl implements LexicalCandidate {

	private String string;
	private int frequency = -1;
	private double relevance = (double)-1;
	private SortedSet<Long> tokenOffsets;
	private SortedSet<Long> chunkOffsets;
	private SortedSet<Long> chunkTokenOffsets;
	
	public LexicalCandidateImpl(String s, int f){
		this.string = s;
		this.frequency = f;
		this.tokenOffsets = new TreeSet<Long>();
		this.chunkOffsets = new TreeSet<Long>();
		this.chunkTokenOffsets = new TreeSet<Long>();
	}
	

	public LexicalCandidateImpl(String s, int f, double r){
		this(s, f);
		this.relevance = r;
	}

	
	public String getStringValue() {
		return string;
	}
	
	
	public int getFrequency() {
		return frequency;
	}

	
	public double getRelevance() {
		return relevance;
	}

	
	public void addTokenOffset(Long start, Long end) {
		tokenOffsets.add(start);
		tokenOffsets.add(end);
	}

	public SortedSet<Long> getTokenOffsets() {
		return tokenOffsets;
	}

	public void addChunkOffset(Long start, Long end) {
		chunkOffsets.add(start);
		chunkOffsets.add(end);
	}

	public SortedSet<Long> getChunkOffsets() {
		return chunkOffsets;
	}

	public void addChunkTokenOffset(Long start, Long end){
		chunkTokenOffsets.add(start);
		chunkTokenOffsets.add(end);
	}
	
	public SortedSet<Long> getChunkTokenOffsets() {
		return chunkTokenOffsets;
	}

	public void print(){
		if (relevance == (double)-1){
			System.out.println( String.format("%1$30s -- %2$5d -- NiR", string, frequency) );
		} else {
			System.out.println( String.format("%1$30s -- %2$5d -- %3$.2f", string, frequency, relevance) );
		}
	}


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((string == null) ? 0 : string.hashCode());
		return result;
	}


	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final LexicalCandidateImpl other = (LexicalCandidateImpl) obj;
		if (string == null) {
			if (other.string != null)
				return false;
		} else if (!string.equals(other.string))
			return false;
		return true;
	}
	
	
}
