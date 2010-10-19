/*
 * 
 * PositionAnalyser.java, provides keyword/keyphrase extraction as a GATE plugin
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


import java.util.logging.Logger;


public class PositionAnalyser {

	private long docStart;
	private long docEnd;
	private static final int PLOT_BASE = 10;
	private int[] weight_array;
	private long[] segment_array;
	private char[] viz_array;
	private String visualisation;
	private boolean hasGlobalScope;
	private boolean hasLocalScope;
	
	private final static char LEFT_BOUNDARY = '[';
	private final static char RIGHT_BOUNDARY = ']';
	private final static char ZERO = ' ';
	private final static char ONE = '.';
	private final static char TWO_TO_NINE = 'x';
	private final static char TEN_OR_MORE = '#';
	
	
	static Logger logger = Logger.getLogger(PositionAnalyser.class.getName());
	
	public PositionAnalyser(long start, long end){
		this.docStart = start;
		this.docEnd = end;
	}
	
	public long getStart(){
		return docStart;
	}
	
	public long getEnd(){
		return docEnd;
	}
	
	public String getVisualisation(){
		StringBuffer buf = new StringBuffer();
		buf.append(LEFT_BOUNDARY);
		for (char c : viz_array){
			buf.append(c);
		}
		buf.append(RIGHT_BOUNDARY);
		
		return buf.toString();
	}
	
	
	/**
	 * visualizes the occurrences of a KeywordOrPhraseCandidate
	 * textually/graphically, analyses the KeywordOrPhraseCandidate
	 * if necessary
	 */
	private void setVisualisation(){

		for (int idx=0; idx<weight_array.length; idx++){
			if (weight_array[idx] == (int)0 ){ // no occurrence, we just paint a whitespace
				viz_array[idx] = ZERO;
			} else { // we set it to true, so we paint it
				double d = Math.log10((double) weight_array[idx]); 
				if ( d == (double)0){ // only 1 occurrence, we paint "."
					viz_array[idx] = ONE;
				} else if (d < (double)1){ // 1-9 occurrences, we paint "x"
					viz_array[idx] = TWO_TO_NINE;
				} else {  // more than 10 occurrences, we paint "#"
					viz_array[idx] = TEN_OR_MORE;
				}
			}
		}
	}
	
	/**
	 * analyses a KeywordOrPhraseCandidate regarding its
	 * occurrences within the document
	 * this analysis is later being used in order to determine
	 * whether the KeywordOrPhraseCandidate is a local or a global
	 * phenomenon for the document, as well for visualisation
	 * also initializes the basic data structures such as
	 * the PLOT[] array and the PLOT_SEGMENTS[] array
	 * @param kw
	 */
	public void analyse(KeywordOrPhraseCandidate kwc){
		init();

		long[] lary = kwc.getOffsetDistribution();
		for (long pos : lary ){
			for (int idx=0; idx<segment_array.length; idx++){
				if (pos <= segment_array[idx]){
					weight_array[idx]++;
					break;
				}
			}
		}
		
		setVisualisation();
		kwc.setDistributionDiagram( getVisualisation() );
		setScope();
		if (this.hasGlobalScope){
			kwc.setGlobalScope();
		}
		if (this.hasLocalScope){
			kwc.setLocalScope();
		}
	}
	
	private void setScope(){
		int count = 0;
		
		// counting number of partitions that have occurrences
		for (char c : viz_array){
			if ( c != ZERO ){
				count++;
			}
		}
		
		// determining whether we have global scope in case half or more of the sections have occurrences
		if (count >= (PLOT_BASE / 2) ){
			int first_half_count = 0;
			for (int idx=0; idx<(viz_array.length/2); idx++){
				if ( viz_array[idx] == ONE || viz_array[idx] == TWO_TO_NINE || viz_array[idx] == TEN_OR_MORE ){
					first_half_count++;
				}
			}
			if (first_half_count >= 1 ){ // if mentioned more than 1 times in the first half, we take it as GlobalScope
				hasGlobalScope = true; // because things that are more important are mentioned at the very beginning
			}
		}
		
		// determining whether we have local scope in case less than half of the sections had occurrences
		if (count < (PLOT_BASE / 2) ){
			int local_count = 0;
			for (int idx=0; idx<viz_array.length; idx++){
				// testing for higher amount of mentions per section than just ONE
				if ( viz_array[idx] != ZERO || viz_array[idx] != ONE ){
					local_count++;
					hasLocalScope = true;
				}
				// testing for occurrences in two consecutive sections
				if (viz_array[idx] != ZERO && idx-1>=0 && viz_array[idx-1] != ZERO ){
					hasLocalScope = true;
				}
			}
		}
	}

	public boolean getGlobalScope(){
		return hasGlobalScope;
	}

	public boolean getLocalScope(){
		return hasLocalScope;
	}

	/**
	 * initializes the PLOT_SEGMENTS array, with PLOT_BASE cells.
	 * each cell holds the upper bound of the char-value it represents,
	 * in a sorted manner.
	 * thus, we can stop by each cell from the start, and mark the
	 * corresponding PLOT cell as <true> if our actual charValue of 
	 * the observed occurrence is lower than the upper bound. this
	 * also has the nice effect that we dont have to look to the
	 * next cell once we have found that the actual charValue is lower
	 * than the upper bound
	 * also initializes the PLOT array, with PLOT_BASE cells.
	 * each cell holds <false> initially (for not being painted)
	 * and is updated to <true> if there is an occurrence in that
	 * part of the document, represented by the corresponding PLOT_SEGMENTS
	 * array
	 */
	private void init(){
		hasGlobalScope = false;
		hasLocalScope = false;
		
		weight_array = new int[PLOT_BASE];
		segment_array = new long[PLOT_BASE];
		viz_array = new char[PLOT_BASE];
		for (int idx=0; idx<PLOT_BASE; idx++){
			segment_array[idx] = (idx+1) * docEnd / PLOT_BASE;
			weight_array[idx] = 0;
			viz_array[idx] = ZERO;
		}
	}

}
