package gate.opennlp;

import gate.AnnotationSet;
import gate.Factory;
import gate.FeatureMap;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import gate.util.InvalidOffsetException;

import java.io.DataInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.zip.GZIPInputStream;

import opennlp.maxent.MaxentModel;
import opennlp.maxent.io.BinaryGISModelReader;
import opennlp.tools.sentdetect.SentenceDetectorME;

import org.apache.log4j.Logger;

/**
 * Wrapper for the open nlp sentence splitter
 * 
 * @author <A
 *         HREF="mailto:georgiev@ontotext.com>georgi.georgiev@ontotext.com</A>
 *         Created: Thu Dec 11 16:25:59 EET 2008
 */

public @SuppressWarnings("all")
class OpenNlpSentenceSplit extends AbstractLanguageAnalyser {

	public static final long serialVersionUID = 1L;

	String inputASName;

	public String getInputASName() {
		return inputASName;
	}

	public void setInputASName(String inputASName) {
		this.inputASName = inputASName;
	}

	private static final Logger logger = Logger
			.getLogger(OpenNlpSentenceSplit.class);

	// private members
	private String annotationSetName = null;
	SentenceDetectorME splitter = null;
	URL model;

	@Override
	public void execute() throws ExecutionException {
		boolean isSentenceSplitted = false;
		// text doc annotations
		AnnotationSet annotations;
		if (annotationSetName != null && annotationSetName.length() > 0)
			annotations = document.getAnnotations(annotationSetName);
		else
			annotations = document.getAnnotations();
		// getdoc.get text
		String text = document.getContent().toString();
		// run tokenizer
		int[] spans = splitter.sentPosDetect(text);
		// compare the resulting
		// sentences and add annotations
		int prevSpan = 0;
		for (int i = 0; i < spans.length; i++) {

			FeatureMap fm = Factory.newFeatureMap();
			// type
			fm.put("source", "openNLP");
			// source
			// fm.put("type", "urn:lsid:ontotext.com:kim:iextraction:Sentence");

			try {
				// annotations.add(Long.valueOf(spans[i].getStart()),
				// Long.valueOf(spans[i].getEnd()), "Sentence", fm);
				// annotations.add(i == 0 ? Long.valueOf(prevSpan) : Long
				// .valueOf(prevSpan + countSpaces(prevSpan - 1)),
				// i == (spans.length - 1) ? Long.valueOf(spans[i]) : Long
				// .valueOf(spans[i] - 1), "Sentence", fm);
				int start = prevSpan;
				int end = spans[i];

				// remove leading spaces of a sentence
				for (int j = start; j < end
						&& Character.isWhitespace(text.charAt(j)); j++) {
					start = j + 1;
				}

				// remove trailing spaces of a sentence
				if (end > 1) {
					for (int j = end; j > start
							&& Character.isWhitespace(text.charAt(j - 1)); j--) {
						end = j - 1;
					}
				}

				annotations.add(Long.valueOf(start), Long.valueOf(end),
						"Sentence", fm);
				if(!isSentenceSplitted)
					isSentenceSplitted = true;

			} catch (InvalidOffsetException e) {
				e.printStackTrace();
				throw new RuntimeException(e);
			}

			prevSpan = spans[i];
		}
		if(!isSentenceSplitted){
			FeatureMap fm = Factory.newFeatureMap();
			// type
			fm.put("source", "openNLP");
			try {
				annotations.add(new Long(0), new Long(text.length()),
						"Sentence", fm);
			} catch (InvalidOffsetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	int countSpaces(int lastSpan) {

		int ws = 0;
		String text = document.getContent().toString();

		char[] context = text.substring(lastSpan - 1,
				text.length() >= lastSpan + 50 ? lastSpan + 50 : text.length())
				.toCharArray();

		for (int i = 0; i < context.length; i++) {
			if (Character.isWhitespace(context[i]))
				ws++;
			else
				break;
		}

		return ws;
	}

	@Override
	public Resource init() throws ResourceInstantiationException {
		// logger.info("Sentence split url is: " + model.getFile());
		try {
			splitter = new SentenceDetectorME(getModel(model));
		} catch (Exception e) {
			logger.error("Sentence Splitter can not be initialized!");
			throw new RuntimeException(
					"Sentence Splitter cannot be initialized!", e);
		}

		logger.warn("Sentence split initialized!");// System.out.println("Sentence split initialized!");

		return this;

	}

	@Override
	public void reInit() throws ResourceInstantiationException {
		init();
	}

	/**
	 * @author joro
	 * @return MaxentModel
	 * @param String
	 *            path to MaxentModel
	 */
	public static MaxentModel getModel(URL name) {
		try {
			return new BinaryGISModelReader(new DataInputStream(
					new GZIPInputStream(name.openStream()))).getModel();
		} catch (IOException E) {
			E.printStackTrace();
			return null;
		}
	}

	/* getters and setters for the PR */
	/* public members */

	public void setAnnotationSetName(String a) {
		annotationSetName = a;
	}

	public String getAnnotationSetName() {
		return annotationSetName;
	}/* getters and setters for the PR */

	public URL getModel() {
		return model;
	}

	public void setModel(URL model) {
		this.model = model;
	}

}
