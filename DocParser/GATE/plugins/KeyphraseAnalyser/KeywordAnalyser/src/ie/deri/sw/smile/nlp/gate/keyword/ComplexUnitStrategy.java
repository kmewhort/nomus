/*
 * 
 * ComplexUnitStrategy.java, provides keyword/keyphrase extraction as a GATE plugin
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


public interface ComplexUnitStrategy {

	
	static final String TOKEN_STOPWORD_FEATURE_VALUE_FALSE = "false";
	static final String TOKEN_STOPWORD_FEATURE_VALUE_TRUE = "true";
	
	static final String TOKEN_KIND_FEATURE_VALUE_NUMBER = "number";
	
	static final String EXTENDED_ALPHANUM_REGEXP_STRING = "[\\w\\s\\d-]";

	static final int ACCEPTABLE_KEYPHRASE_MIN_CHARLENGTH = 2;
	static final int ACCEPTABLE_KEYPHRASE_MAX_TOKEN_SIZE = 5;

	static final String DASH_STRING = "-";

	static final int NGRAM_MIN_N = 2;
	static final int NGRAM_MAX_N = 4;

	public Set<ComplexTerm> produceCandidateTerms(LexicalCandidateStore lexicalCandidateStore);

}
