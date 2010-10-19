/*
 * 
 * MutualInformationMeasure.java, provides keyword/keyphrase extraction as a GATE plugin
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

import java.util.logging.Logger;

import gate.creole.ExecutionException;
import ie.deri.sw.smile.nlp.gate.keyword.KeywordAnalyser;
import ie.deri.sw.smile.nlp.gate.keyword.LexicalRelevanceMeasure;

public class MutualInformationMeasure extends RelevanceMeasure implements
		LexicalRelevanceMeasure {
	
	private static Logger logger = Logger.getLogger(MutualInformationMeasure.class
			.getName());

	public MutualInformationMeasure(int docSize, int refCorpusSize) {
		super();
		setDocumentSize(docSize);
		setReferenceCorpusSize(refCorpusSize);
	}

	public double compute(int documentFrequency, int refCorpusFrequency) throws ExecutionException {

		if (refCorpusFrequency == 0){
			throw new ExecutionException("cannot compute value without observation in reference corpus");
			
		} else {
			float observed = (float) documentFrequency;
			float expected = ((float) refCorpusFrequency / (float) document_size) * (float) documentFrequency;
			
			return (float)Math.log(observed / expected) / (float)Math.log(2.0);
		}
	}

	public static void main(String[] args){
		MutualInformationMeasure x2 = new MutualInformationMeasure(3000, 181376006);
		try {
			System.out.println( String.format("and: %1$.2f", x2.compute(100, (int)(23546.30*181.376006)) ) );
		} catch (Exception exn){
			exn.printStackTrace();
		}
	}

}
