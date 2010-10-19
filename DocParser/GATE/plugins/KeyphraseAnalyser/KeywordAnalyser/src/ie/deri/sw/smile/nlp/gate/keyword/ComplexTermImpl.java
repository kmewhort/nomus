/*
 * 
 * ComplexTermImpl.java, provides keyword/keyphrase extraction as a GATE plugin
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
import java.util.Iterator;
import java.util.List;

import com.wcohen.ss.AbstractStringDistance;

public class ComplexTermImpl implements ComplexTerm {

	private boolean mark = false;
	private List<String> cueTokens;
	private String stringValue = "";
	private List<String> tokenComponents;
	private List<String> lemmaComponents;
	private long startOffset;
	private long endOffset;
	private double cueTokenSignificanceValue = 0.0;
	
	public ComplexTermImpl(long startOffset, long endOffset){
		this.startOffset = startOffset;
		this.endOffset = endOffset;
		cueTokens = new ArrayList<String>();
		tokenComponents = new ArrayList<String>();
		lemmaComponents = new ArrayList<String>();
	}

	
	public void computeStringValue(){
		Iterator sItr = tokenComponents.iterator();
		while (sItr.hasNext()){
			stringValue += sItr.next();
			if (sItr.hasNext()){
				stringValue += " ";
			}
		}
	}
	
	public String getStringValue() {
		return stringValue;
	}
	
	
	public void addCueToken(String s){
		cueTokens.add(s);
	}
	
	public String[] getCueTokens() {
		return cueTokens.toArray(new String[cueTokens.size()]);
	}
	
	public void addCueTokenSignificanceValue(double sig) {
		cueTokenSignificanceValue += sig;
	};
	
	public double getCueTokenSignificanceValue(){
		return cueTokenSignificanceValue;
	}
	
	public void addTokenComponent(String token){
		tokenComponents.add(token);
	}
	
	public String[] getTokenComponents(){
		return tokenComponents.toArray(new String[tokenComponents.size()]);
	}

	
	public void addLemmaComponent(String lemma){
		lemmaComponents.add(lemma);
	}
	
	public String[] getLemmaComponents(){
		return lemmaComponents.toArray(new String[lemmaComponents.size()]);
	}
	
	public void setStartOffset(long start){
		this.startOffset = start;
	}
	
	public long getStartOffset(){
		return startOffset;
	}
	
	public void setEndOffset(long end){
		this.endOffset = end;
	}
	
	public long getEndOffset(){
		return endOffset;
	}

	public boolean isMarked() {
		return mark;
	}

	public void mark() {
		this.mark = true;
	}

	public void unmark() {
		this.mark = false; 
	}

	
	public boolean isStrikinglySimilar(AbstractStringDistance metric, ComplexTerm otherTerm, double threshold) {
		if (metric.score(Arrays.toString(this.getLemmaComponents()), Arrays
				.toString(otherTerm.getLemmaComponents() )) > threshold ) {
			return true;
		}
		return false;
	}
	
	
	// need to override equals solely based on start-offset and end-offset
	// because we want to avoid considering the same observation from a different 
	// perspective twice for one document, 
	// i.e. if perspective is "semantic", 
	// then we observe [cue:"semantic",stringval:"semantic similarity",start:300,end:319]
	// but if the perspective is "similarity", we also might 
	// observe [cue:"similiarity",stringval:"semantic similarity",start:300,end:319]
	// thats why we implement equality by start/end offset
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (endOffset ^ (endOffset >>> 32));
		result = prime * result + (int) (startOffset ^ (startOffset >>> 32));
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
		final ComplexTermImpl other = (ComplexTermImpl) obj;
		if (endOffset != other.endOffset)
			return false;
		if (startOffset != other.startOffset)
			return false;
		return true;
	}

}
