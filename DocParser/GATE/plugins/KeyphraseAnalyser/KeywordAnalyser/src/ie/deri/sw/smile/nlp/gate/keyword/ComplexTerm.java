/*
 * 
 * ComplexTerm.java, provides keyword/keyphrase extraction as a GATE plugin
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

import com.wcohen.ss.AbstractStringDistance;

public interface ComplexTerm {

	public void addCueToken(String token);
	
	public void addCueTokenSignificanceValue(double sig);
	
	public double getCueTokenSignificanceValue();
	
	public String[] getCueTokens();
	
	public void computeStringValue();
	
	public String getStringValue();
	
	public void addTokenComponent(String token);
	
	public String[] getTokenComponents();
	
	public void addLemmaComponent(String lemma);
	
	public String[] getLemmaComponents();
	
	public boolean isStrikinglySimilar(AbstractStringDistance metric, ComplexTerm otherTerm, double threshold);
	
	public void setStartOffset(long start);
	
	public long getStartOffset();
	
	public void setEndOffset(long end);
	
	public long getEndOffset();
	
	public void mark();
	
	public void unmark();
	
	public boolean isMarked();
	
}
