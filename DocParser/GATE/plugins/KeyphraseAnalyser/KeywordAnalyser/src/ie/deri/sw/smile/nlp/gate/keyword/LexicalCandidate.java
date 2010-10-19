/*
 * 
 * LexicalCandidate.java, provides keyword/keyphrase extraction as a GATE plugin
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

import java.util.SortedSet;

public interface LexicalCandidate {

	public String getStringValue();
	
	public int getFrequency();
	
	public double getRelevance();
	
	public void addTokenOffset(Long start, Long end);
	
	public SortedSet<Long> getTokenOffsets();
	
	public void addChunkOffset(Long start, Long end);
	
	public SortedSet<Long> getChunkOffsets();
	
	public void addChunkTokenOffset(Long start, Long end);
	
	public SortedSet<Long> getChunkTokenOffsets();
	
	public void print();
	
}
