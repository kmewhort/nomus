/*
 * 
 * ChiSquaredMeasure.java, provides keyword/keyphrase extraction as a GATE plugin
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

public class ChiSquaredMeasure extends RelevanceMeasure implements
		LexicalRelevanceMeasure {

	private static Logger logger = Logger.getLogger(ChiSquaredMeasure.class
			.getName());

	
	public ChiSquaredMeasure(int docSize, int refCorpusSize) {
		super();
		setDocumentSize(docSize);
		setReferenceCorpusSize(refCorpusSize);
	}

	public double compute(int documentFrequency, int refCorpusFrequency)
			throws ExecutionException {

		if (refCorpusFrequency == 0) {
			throw new ExecutionException(
					"cannot compute value without observation in reference corpus");

		} else {
			double observed = (double) documentFrequency;
			double expected = ((double) refCorpusFrequency / (double) document_size)
					* (double) documentFrequency;

			return computeChiSquared((double) document_size
					+ reference_corpus_size, observed,
					(double) refCorpusFrequency,
					(double) (document_size - observed),
					(double) (reference_corpus_size - refCorpusFrequency) );
		}

	}

	// document_frequency = f
	// referemce_frequency = F
	// document_size = n
	// reference_corpus_size = N
	private double computeChiSquared(double N, double o11, double o12, double o21,
			double o22) {

		double exp_o11 = (o11 + o12) * (o11 + o21) / N;
		double exp_o12 = (o12 + o11) * (o12 + o22) / N;
		double exp_o21 = (o21 + o11) * (o21 + o22) / N;
		double exp_o22 = (o22 + o12) * (o22 + o21) / N;

//		System.out.println(String.format("    o11: %1$12d,     o12: %2$12d" , (int)o11, (int)o12));
//		System.out.println(String.format("    o21: %1$12d,     o22: %2$12d" , (int)o21, (int)o22));
//		System.out.println();
//		System.out.println(String.format("exp_o11: %1$12.2f, exp_o12: %2$12.2f" , exp_o11, exp_o12));
//		System.out.println(String.format("exp_o21: %1$12.2f, exp_o22: %2$12.2f" , exp_o21, exp_o22));
		
		double x2 = (double) ((Math.pow((double) (o11 - exp_o11), (double) 2) / exp_o11)
				+ (Math.pow((double) (o12 - exp_o12), (double) 2) / exp_o12)
				+ (Math.pow((double) (o21 - exp_o21), (double) 2) / exp_o21) + (Math
				.pow((double) (o22 - exp_o22), (double) 2) / exp_o22));

		// the simplified formula for 2x2 contingency tables
//		 double x2 = (N * (o11 * o22 - o12 * o21) * (o11 * o22 - o12 * o21))
//		 / ((o11 + o12) * (o11 + o21) * (o12 + o22) * (o21 + o22));

		return x2;
	}

	public static void main(String[] args) {
		int docSize = 2937;
		int refCSize = 181376006;
		ChiSquaredMeasure x2 = new ChiSquaredMeasure(docSize, refCSize);
		try {
			System.out.println(String.format("pattern: %1$.2f", x2.compute(9,
					(int) (108.06 * (double)refCSize/1000000))));
		} catch (Exception exn) {
			exn.printStackTrace();
		}
	}
}
