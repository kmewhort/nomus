package gate.opennlp;

import java.io.DataInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.zip.GZIPInputStream;

import org.apache.log4j.Logger;

import opennlp.maxent.MaxentModel;
import opennlp.maxent.io.BinaryGISModelReader;
import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.util.Span;
import gate.Annotation;
import gate.AnnotationSet;
import gate.Factory;
import gate.FeatureMap;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import gate.util.InvalidOffsetException;

/**
 * Wrapper for the opennlp tokenizer
 * @author <A HREF="mailto:georgiev@ontotext.com>georgi.georgiev@ontotext.com</A>
 * Created: Thu Dec 11 16:25:59 EET 2008
 */

public @SuppressWarnings("all") class OpenNlpTokenizer extends AbstractLanguageAnalyser {

	public static final long serialVersionUID = 1L;
	
	private static final Logger logger = Logger.getLogger(OpenNlpTokenizer.class);
	
	String inputASName;

	public String getInputASName() {
		return inputASName;
	}

	public void setInputASName(String inputASName) {
		this.inputASName = inputASName;
	}

	// private members
	private String annotationSetName = null;
	TokenizerME tokenizer = null;
	URL model;

	@Override
	public void execute() throws ExecutionException {
		// text doc annotations
		AnnotationSet annotations;
		if (annotationSetName != null && annotationSetName.length() > 0)
			annotations = document.getAnnotations(annotationSetName);
		else
			annotations = document.getAnnotations();

		// get sentence annotations
		//AnnotationSet sentences = document.getAnnotations("Sentence");

		// getdoc.get text
		String text = document.getContent().toString();
		// run tokenizer
		Span[] spans = tokenizer.tokenizePos(text);
		// compare the resulting
		// spans and add annotations

		for (int i = 0; i < spans.length; i++) {

			FeatureMap fm = Factory.newFeatureMap();
			// type
			fm.put("source", "openNLP");
			fm.put("string", text.substring(spans[i].getStart(), spans[i]
					.getEnd()));
			// source
//			fm.put("type", "urn:lsid:ontotext.com:kim:iextraction:Token");

			try {
				annotations.add(Long.valueOf(spans[i].getStart()), Long
						.valueOf(spans[i].getEnd()), "Token", fm);

			} catch (InvalidOffsetException e) {
				e.printStackTrace();
				throw new RuntimeException(e);
			}

			//startSpan = spans[i].getEnd() + countSpaces(spans[i].getEnd());
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
		//logger.info("The string of Tokenizer file is: "+model);
		tokenizer = new TokenizerME(
				getModel(model));
		
		logger.warn("OpenNLP Tokenizer initialized!");//System.out.println("OpenNLP Tokenizer initialized!");
		
		return this;

	}

	@Override
	public void reInit() throws ResourceInstantiationException {
		init();
	}

	/**
	 * @author georgiev
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
			logger.error("OpenNLP Tokenizer can not be initialized!");
			throw new RuntimeException("OpenNLP Tokenizer can not be initialized!", E);
		}
	}

	/* getters and setters for the PR */
	/* public members */
	public void setAnnotationSetName(String a) {
		annotationSetName = a;
	}

	public String getAnnotationSetName() {
		return annotationSetName;
	}

	public URL getModel() {
		return model;
	}

	public void setModel(URL model) {

		this.model = model;
	}/* getters and setters for the PR */

}
