package gate.opennlp;

import gate.Annotation;
import gate.AnnotationSet;
import gate.FeatureMap;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;

import java.io.DataInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.zip.GZIPInputStream;

import opennlp.maxent.MaxentModel;
import opennlp.maxent.io.BinaryGISModelReader;
import opennlp.tools.chunker.ChunkerME;

import org.apache.log4j.Logger;

/**
 * Wrapper for the opennlp chuncker
 * 
 * @author <A
 *         HREF="mailto:georgiev@ontotext.com">georgi.georgiev@ontotext.com</A>
 *         Created: Thu Dec 11 16:25:59 EET 2008
 */

public @SuppressWarnings("all")
class OpenNlpChunker extends AbstractLanguageAnalyser {

	public static final long serialVersionUID = 1L;

	private static final Logger logger = Logger.getLogger(OpenNlpChunker.class);

	String inputASName;

	public String getInputASName() {
		return inputASName;
	}

	public void setInputASName(String inputASName) {
		this.inputASName = inputASName;
	}

	/**
	 * @author georgi georgiev
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

	// private members
	private String annotationSetName = null;
	ChunkerME chunker = null;

	private URL model;

	@SuppressWarnings("unchecked")
	@Override
	public void execute() throws ExecutionException {
		// text doc annotations
		AnnotationSet annotations;
		if (annotationSetName != null && annotationSetName.length() > 0) {
			annotations = document.getAnnotations(annotationSetName);
		} else {
			annotations = document.getAnnotations();
		}

		// getdoc.get text
		// String text = document.getContent().toString();

		// get token and sentence annotations
		AnnotationSet sentences = annotations.get("Sentence");
		AnnotationSet tokensAS = annotations.get("Token");

		if (sentences != null && sentences.size() > 0 && tokensAS != null
				&& tokensAS.size() > 0) {

			// order them
			List<Annotation> sentList = new LinkedList<Annotation>();

			for (Iterator iterator = sentences.iterator(); iterator.hasNext();) {
				sentList.add((Annotation) iterator.next());

			}

			java.util.Collections.sort(sentList,
					new gate.util.OffsetComparator());

			// for each sentence get token annotations
			for (Iterator iterator = sentList.iterator(); iterator.hasNext();) {
				Annotation annotation = (Annotation) iterator.next();

				AnnotationSet sentenceTokens = annotations.get("Token",
						annotation.getStartNode().getOffset(), annotation
								.getEndNode().getOffset());

				// create a list

				List<Annotation> annList = new LinkedList<Annotation>();

				for (Iterator<Annotation> iterator2 = sentenceTokens.iterator(); iterator2
						.hasNext();) {
					annList.add(iterator2.next());

				}

				// order on offset
				Collections.sort(annList, new gate.util.OffsetComparator());

				// make the array be string[] sentence
				String[] tokens = new String[sentenceTokens.size()];
				String[] postags = new String[sentenceTokens.size()];
				int i = 0;
				for (Iterator iterator3 = annList.iterator(); iterator3
						.hasNext();) {

					Annotation token = (Annotation) iterator3.next();

					tokens[i] = token.getFeatures().get("string").toString();
					Object posObj = token.getFeatures().get("category");
					if (posObj == null) {
						throw new ExecutionException(
								"Token found with no POS tag category!\n"
										+ "Did you run a POS tagger before the OpenNLPChunker?");
					}
					postags[i] = posObj.toString();

					i++;
				}

				// run pos chunker
				String[] chunks = chunker.chunk(tokens, postags);

				// add tohose chunk tags to token annotations

				int j = 0;

				for (Iterator iterator4 = annList.iterator(); iterator4
						.hasNext();) {
					Annotation token = (Annotation) iterator4.next();

					FeatureMap fm = token.getFeatures();
					fm.put("chunk", chunks[j]);

					token.setFeatures(fm);

					j++;

				}
			}
		} else {
			throw new ExecutionException("No sentences or tokens to process!\n"
					+ "Please run a sentence splitter "
					+ "and tokeniser first!");

		}
	}

	public String getAnnotationSetName() {
		return annotationSetName;
	}

	public URL getModel() {
		return model;
	}

	/* getters and setters for the PR */
	/* public members */

	@Override
	public Resource init() throws ResourceInstantiationException {
		// logger.error("Chunker url is: " + model.getFile());
		try {
			chunker = new ChunkerME(getModel(model));
			// "/home/joro/work/m_learning/models/Chunker_Genia.bin.gz"));
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Chunker can not be initialized!");
			throw new RuntimeException("Chunker cannot be initialized!", e);
		}

		return this;

	}

	@Override
	public void reInit() throws ResourceInstantiationException {
		init();
	}

	public void setAnnotationSetName(String a) {
		annotationSetName = a;
	}

	public void setModel(URL model) {
		this.model = model;
	}

}
