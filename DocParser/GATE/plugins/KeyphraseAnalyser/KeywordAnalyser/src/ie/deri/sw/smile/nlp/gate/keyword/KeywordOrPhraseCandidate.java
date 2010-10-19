/*
 * 
 * KeywordOrPhraseCandidate.java, provides keyword/keyphrase extraction as a GATE plugin
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


import java.util.Set;

public interface KeywordOrPhraseCandidate  {

	public String[] getCueTokens();

	public void setCueTokens(String[] cueTokens);

	/**
	 * the string value of the cluster of complex terms it represents
	 * 
	 * @return
	 */
	public String getStringValue();
	

	
	public void setClusterStrings(Set<ComplexTerm> cluster);

	/**
	 * the frequency of occurring complex terms this KeywordOrPhraseCandidate is
	 * representing
	 * @return
	 */
	public int getFrequency();
	
	public void setFrequency(int f);
	

	/**
	 * the maximised significance over the complex terms underlying this KeywordOrPhraseCandidate
	 * @return
	 */
	public double getSignificance();
	
	public void setSignificance(double sig);
	
	
	/**
	 * the confidence (sic)
	 * 
	 * @return
	 */
	public double getConfidence();

	public void setConfidence(double c);
	
	
	
	/**
	 * contains only the StartOffsets of occurrences of the respective KeywordOrPhraseCandidate
	 * @return
	 */
	public long[] getOffsetDistribution();
	
	public void setOffsetDistribution(long[] distributionArray);

	
	public void setDistributionDiagram(String diagram);
	
	/**
	 * a visual representation of where the KeywordOrPhraseCandidate occurs
	 * within the document
	 * @return
	 */
	public String getDistributionDiagram();


	
	/**
	 * depending on the distribution, this reflects whether the
	 * KeywordOrPhraseCandidate can be observed throughout the document
	 * <p>
	 * intuitively, (!hasGlobalScope && !hasLocalScope) should not be regarded as
	 * KeywordOrPhraseCandidate
	 * @return
	 */
	public boolean hasGlobalScope();
	
	public void setGlobalScope();
	

	/**
	 * depending on the distribution, this reflects whether the
	 * KeywordOrPhraseCandidate can be observed only in restricted parts of the
	 * document
	 * <p>
	 * intuitively, (!hasGlobalScope && !hasLocalScope) should not be regarded as
	 * KeywordOrPhraseCandidate
	 * @return
	 */
	public boolean hasLocalScope();
	
	public void setLocalScope();



	/**
	 * 
	 */
	public void print();
	
}
