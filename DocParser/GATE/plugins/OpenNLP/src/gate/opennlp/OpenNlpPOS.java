package gate.opennlp;

import gate.Annotation;
import gate.AnnotationSet;
import gate.FeatureMap;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.zip.GZIPInputStream;

import opennlp.maxent.MaxentModel;
import opennlp.maxent.io.BinaryGISModelReader;
import opennlp.tools.postag.POSDictionary;
import opennlp.tools.postag.POSTaggerME;

import org.apache.log4j.Logger;

/**
 * Wrapper for the open nlp pos tagger
 * 
 * @author <A
 *         HREF="mailto:georgiev@ontotext.com">georgi.georgiev@ontotext.com</A>
 *         Created: Thu Dec 11 16:25:59 EET 2008
 */

public @SuppressWarnings("all")
class OpenNlpPOS extends AbstractLanguageAnalyser {

	public static final long serialVersionUID = 1L;

	private static final Logger logger = Logger.getLogger(OpenNlpPOS.class);

	String inputASName;

	public String getInputASName() {
		return inputASName;
	}

	public void setInputASName(String inputASName) {
		this.inputASName = inputASName;
	}

	// private members
	private String annotationSetName = null;
	POSTaggerME pos = null;
	URL model;
	URL dictionary;
	private String dictionaryEncoding = "UTF-8";

	@Override
	public void execute() throws ExecutionException {
		// text doc annotations
		AnnotationSet annotations;
		if (annotationSetName != null && annotationSetName.length() > 0)
			annotations = document.getAnnotations(annotationSetName);
		else
			annotations = document.getAnnotations();

		// getdoc.get text
		String text = document.getContent().toString();

		// get sentence annotations
		AnnotationSet sentences = annotations.get("Sentence");

		// order sentences

		List<Annotation> sentList = new LinkedList<Annotation>();

		for (Iterator iterator = sentences.iterator(); iterator.hasNext();) {
			sentList.add((Annotation) iterator.next());

		}

		java.util.Collections.sort(sentList, new gate.util.OffsetComparator());

		// for each sentence get token annotations
		for (Iterator iterator = sentList.iterator(); iterator.hasNext();) {
			Annotation annotation = (Annotation) iterator.next();

			AnnotationSet sentenceTokens = annotations.get("Token", annotation
					.getStartNode().getOffset(), annotation.getEndNode()
					.getOffset());

			// create a list

			List<Annotation> tokenList = new LinkedList<Annotation>();

			for (Iterator iterator2 = sentenceTokens.iterator(); iterator2
					.hasNext();) {
				tokenList.add((Annotation) iterator2.next());

			}

			// order on offset

			Collections.sort(tokenList, new gate.util.OffsetComparator());

			// make the array be string[] sentence
			String[] sentence = new String[tokenList.size()];
			int i = 0;
			for (Iterator iterator2 = tokenList.iterator(); iterator2.hasNext();) {

				Annotation token = (Annotation) iterator2.next();

				sentence[i] = token.getFeatures().get("string").toString()
						.replaceAll("\\s+", "").trim();

				i++;
			}

			StringBuffer buf = new StringBuffer();
			for (int j = 0; j < sentence.length; j++) {
				buf.append(sentence[j] + "@@@");
			}

			// run pos tagger
			String[] postags = null;
			/**
			 * we will make shure to not allow smth to breack the tagger
			 */
			try {
				postags = pos.tag(sentence);
			} catch (Exception e) {
				e.printStackTrace();
				System.out
						.println("There is a problem....\n with this sentence");
				System.out.println(buf);
				continue;
			}

			// add tohose spans to token annotations

			int j = 0;
			for (Iterator iterator2 = tokenList.iterator(); iterator2.hasNext();) {
				Annotation token = (Annotation) iterator2.next();

				FeatureMap fm = token.getFeatures();
				fm.put("category", postags[j]);

				token.setFeatures(fm);

				j++;

			}
		}
	}

	@Override
	public Resource init() throws ResourceInstantiationException {
		// logger.warn("OpenNLP POS initializing strings are: model - " +
		// model.getFile() +
		// " dictionary: "+dictionary.getFile());
		try {
			BufferedReader dictionaryReader = new BufferedReader(
					new InputStreamReader(dictionary.openStream(),
							dictionaryEncoding));
			pos = new POSTaggerME(getModel(model), new POSDictionary(
					dictionaryReader, true));
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("OpenNLP POS can not be initialized!");
			throw new RuntimeException("OpenNLP POS can not be initialized!", e);
		}
		logger.warn("OpenNLP POS initialized!");// System.out.println("OpenNLP POS initialized!");
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
	}

	public URL getModel() {
		return model;
	}

	public void setModel(URL model) {
		this.model = model;
	}

	public URL getDictionary() {
		return dictionary;
	}

	public void setDictionary(URL dictionary) {
		this.dictionary = dictionary;
	}

	public void setDictionaryEncoding(String a) {
		dictionaryEncoding = a;
	}

	public String getDictionaryEncoding() {
		return dictionaryEncoding;
	}

}
