/*
 * 
 * KeywordOrPhraseCandidateImpl.java, provides keyword/keyphrase extraction as a GATE plugin
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


import java.util.Arrays;
import java.util.Iterator;
import java.util.Set;

public class KeywordOrPhraseCandidateImpl implements KeywordOrPhraseCandidate {

	private String[] cueTokens;
	private String stringValue;
	private long[] offsetDistribution;
	private String distributionDiagram;
	private String[] clusterStrings;

	private double confidence = -1D;
	private double significance = 0.0;
	private int frequency = -1;
	private boolean globalScope = false;
	private boolean localScope = false;

	
	public KeywordOrPhraseCandidateImpl(String stringValue){
		this.stringValue = stringValue;
	}

	
	
	public String getStringValue() {
		return stringValue;
	}

	

	public void setCueTokens(String[] cueTokens) {
		this.cueTokens = cueTokens;
	}
	
	public String[] getCueTokens() {
		return cueTokens;
	}
	

	public void setClusterStrings(Set<ComplexTerm> cluster){
		this.clusterStrings = new String[cluster.size()];
		Iterator<ComplexTerm> ctItr = cluster.iterator();
		int idx = 0;
		while (ctItr.hasNext() && idx < cluster.size()){
			clusterStrings[idx++] = ctItr.next().getStringValue();
		}
		
	}
	
	public void setSignificance(double sig){
		this.significance = sig;
	}
	
	public double getSignificance(){
		return this.significance;
	}
	
	
	public void setConfidence(double c) {
		this.confidence = c;		
	}

	public double getConfidence() {
		return confidence;
	}

	
	
	public void setFrequency(int f) {
		this.frequency = f;
	}
	
	public int getFrequency() {
		return frequency;
	}

	
	
	public void setOffsetDistribution(long[] distributionArray) {
		Arrays.sort(distributionArray); 
		this.offsetDistribution = distributionArray;
	}
	
	public long[] getOffsetDistribution() {
		return offsetDistribution;
	}

	
	public void setDistributionDiagram(String diagram){
		this.distributionDiagram = diagram;
	}
	

	
	
	public void setGlobalScope() {
		this.globalScope = true;
	}
	
	public boolean hasGlobalScope() {
		return globalScope;
	}

	
	
	public void setLocalScope() {
		this.localScope = true;
	}
	
	public boolean hasLocalScope() {
		return localScope;
	}

	
	public String getDistributionDiagram() {
		return distributionDiagram;
	}
	
	
	public void print(){
		StringBuffer sbuf = new StringBuffer();
		
		sbuf.append( stringValue ).append(" -- ");
		sbuf.append(" CONF: ").append( String.format("%1$.3f", confidence) ).append(" -- ");
		sbuf.append(" FREQ: ").append(frequency).append(" -- ");
		sbuf.append(" SIG: ").append(String.format("%1$.3f",significance)).append(" -- ");
		sbuf.append( Arrays.toString(cueTokens) ).append(" -- ");
		sbuf.append( distributionDiagram ).append(" -- ");
		sbuf.append( "SCOPE: " );
		sbuf.append( globalScope ? "global" : localScope ? "local" : "no-scope" );
		sbuf.append(Arrays.toString(clusterStrings));
		
		System.out.println(sbuf.toString());
		sbuf.delete(0, sbuf.length());
		sbuf = null;
	}



	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(confidence);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + Arrays.hashCode(cueTokens);
		result = prime
				* result
				+ ((distributionDiagram == null) ? 0 : distributionDiagram
						.hashCode());
		result = prime * result + frequency;
		result = prime * result + (globalScope ? 1231 : 1237);
		result = prime * result + (localScope ? 1231 : 1237);
		result = prime * result + Arrays.hashCode(offsetDistribution);
		result = prime * result
				+ ((stringValue == null) ? 0 : stringValue.hashCode());
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
		final KeywordOrPhraseCandidateImpl other = (KeywordOrPhraseCandidateImpl) obj;
		if (Double.doubleToLongBits(confidence) != Double
				.doubleToLongBits(other.confidence))
			return false;
		if (!Arrays.equals(cueTokens, other.cueTokens))
			return false;
		if (distributionDiagram == null) {
			if (other.distributionDiagram != null)
				return false;
		} else if (!distributionDiagram.equals(other.distributionDiagram))
			return false;
		if (frequency != other.frequency)
			return false;
		if (globalScope != other.globalScope)
			return false;
		if (localScope != other.localScope)
			return false;
		if (!Arrays.equals(offsetDistribution, other.offsetDistribution))
			return false;
		if (stringValue == null) {
			if (other.stringValue != null)
				return false;
		} else if (!stringValue.equals(other.stringValue))
			return false;
		return true;
	}
	
}

