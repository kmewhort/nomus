/*
 * 
 * RelevanceMeasure.java, provides keyword/keyphrase extraction as a GATE plugin
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

package ie.deri.sw.smile.nlp.gate.keyword.measure;

import gate.creole.ExecutionException;
import ie.deri.sw.smile.nlp.gate.keyword.LexicalRelevanceMeasure;

public abstract class RelevanceMeasure implements LexicalRelevanceMeasure {

	protected int reference_corpus_size = -1;
	protected int reference_lexicon_size = -1;
	protected int document_size = -1;
	protected int lexicon_size = -1;

	public final void setReferenceCorpusSize(int size){
		this.reference_corpus_size = size;
	}
	
	public final void setReferenceLexiconSize(int size){
		this.reference_lexicon_size = size;
	}
	
	public final void setDocumentSize(int size){
		this.document_size = size;
	}

	public final void setLexiconSize(int size){
		this.lexicon_size = size;
	}
	
	public abstract double compute(int documentFrequency, int refCorpusFrequency) throws ExecutionException;
}
