/*
 * 
 * PosTagMapper.java, provides mapping from part-of-speech tags to 
 * universal categories as a GATE plugin
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

package ie.deri.sw.smile.nlp.gate.postag.mapper;

import gate.Annotation;
import gate.AnnotationSet;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class is the implementation of the resource POSTAGMAPPER.
 */
public class PosTagMapper extends AbstractLanguageAnalyser {

	private static final long serialVersionUID = 7050879298705781523L;

	private Map<String, Map<String, String>> lang2tagsetmappingMap = null;

	private static final String TAGSET_MAPPING_FILE_PREFIX = "tagsetmapping_";

	private static final String TAGSET_MAPPING_FILE_SUFFIX = ".lst";

	private static Logger logger = Logger.getLogger(PosTagMapper.class
			.getName());

	public PosTagMapper() {
		this.lang2tagsetmappingMap = new HashMap<String, Map<String, String>>();
	}

	public Resource init() throws ResourceInstantiationException {

		if (resourceDir == null) {
			throw new ResourceInstantiationException(
					"no resource directory provided..");
		}
		// check whether file conforms to naming conventions
		java.io.File[] tagsetMappingFiles = new java.io.File(resourceDir
				.getFile()).listFiles(new java.io.FileFilter() {

			public boolean accept(File pathname) {
				String filename = pathname.getName();
				if (filename.startsWith(TAGSET_MAPPING_FILE_PREFIX)
						&& filename.endsWith(TAGSET_MAPPING_FILE_SUFFIX)) {
					return true;
				}
				return false;
			}
		});

		// parse language from filename
		// populate language specific tagset mapping map
		// and store in map with language as key
		StringBuilder languages = new StringBuilder();
		for (int i = 0; i < tagsetMappingFiles.length; i++) {

			String language = tagsetMappingFiles[i].getName().substring(14, 16);

			Map<String, String> tagsetMappingMap = new HashMap<String, String>();

			BufferedReader reader = null;
			try {

				reader = new BufferedReader(new FileReader(
						tagsetMappingFiles[i]));
				while (reader.ready()) {
					String line = null;
					if ((line = reader.readLine()) != null) {
						// exclude lines not conforming to map-format,
						// i.e. not containing SOURCE TAG --> TARGET TAG
						String[] mapping = line.split(" --> ");
						// we also want to control whether the target tag is an
						// acceptable one
						if (mapping.length == 2) {
							tagsetMappingMap.put(mapping[0].trim(), mapping[1]
									.trim());
						}
					}
				}

				// storing populated pos-tag set in hash with language as key
				lang2tagsetmappingMap.put(language, tagsetMappingMap);
				languages.append(language).append(",");

			} catch (FileNotFoundException exn) {
				logger.log(Level.WARNING, exn.getMessage(), exn);
			} catch (IOException exn) {
				logger.log(Level.WARNING, exn.getMessage(), exn);
			} finally { // closing filehandle
				try {
					reader.close();
				} catch (IOException exn) {
					logger.log(Level.WARNING,
							"could not close reader, leaving open filehandle for :"
									+ tagsetMappingFiles[i] + " -- "
									+ exn.getMessage(), exn);
				}
				reader = null;
			}
		}
		logger.log(Level.INFO, PosTagMapper.class.getName()
				+ ": tagset mappings added: "
				+ languages.subSequence(0, languages.length() - 1).toString());

		// clean up
		languages.delete(0, languages.length());
		languages = null;
		tagsetMappingFiles = null;

		return this;
	}

	public void execute() throws ExecutionException {

		String language = null;
		Map<String, String> tagsetMapping = null;
		AnnotationSet inputAS = null;
		AnnotationSet tokens = null;

		try {

			// check if we have a document to process
			if (document == null) {
				throw new ExecutionException("no document to process..");
			}

			// check if we have identified the language of the document
			if (!document.getFeatures().containsKey(languageFeatureName)) {
				throw new ExecutionException(
						"no language identified for document"
								+ document.getName()
								+ " , run LanguageIdentifier first and then Tokeniser ");
			}

			// get the detected language of the document
			language = (String) document.getFeatures().get(languageFeatureName);

			// check if we have a dedicated stopword list for the identified
			// language
			if (!this.lang2tagsetmappingMap.containsKey(language)) {
				throw new ExecutionException(
						"language not supported for tagset mapping");
			}

			// get the dedicated stopword list for the detected language
			tagsetMapping = this.lang2tagsetmappingMap.get(language);

			// get the annotationSet name provided by the user, or
			// otherwise use the default method
			inputAS = (annotationSetName == null || annotationSetName.length() == 0) ? document
					.getAnnotations()
					: document.getAnnotations(annotationSetName);

			// get tokens of documents and enrich them with stopword
			// information
			tokens = inputAS.get(TOKEN_ANNOTATION_TYPE);
			if (tokens == null || tokens.isEmpty()) {

				throw new ExecutionException(
						document.getName()
								+ " does not have any content for token or run the Tokeniser first and then LanguageIdentifier");
			}

			Iterator<Annotation> tokenIter = tokens.iterator();
			// process tokens
			while (tokenIter != null && tokenIter.hasNext()) {
				Annotation currentToken = tokenIter.next();

				if (!currentToken.getFeatures().containsKey(
						TOKEN_CATEGORY_FEATURE_NAME)) {
					throw new Exception(
							document.getName()
									+ " does not have part-of-speech information. Run a POS-Tagger first..");
				}

				// get Token category / part of speech tag
				String tokenPOSTag = (String) currentToken.getFeatures().get(
						TOKEN_CATEGORY_FEATURE_NAME);

				// check whether token string is in set of stopwords,
				// and add the feature
				if (tagsetMapping.containsKey(tokenPOSTag)) {
					currentToken.getFeatures().put(coarseCategoryFeatureName,
							tagsetMapping.get(tokenPOSTag));
				}
				tokenPOSTag = null;
			}
			tokenIter = null;

		} catch (Exception exn) {
			throw new ExecutionException("execption in "
					+ PosTagMapper.class.getName(), exn);
		}
	}

	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

	public String getEncoding() {
		return this.encoding;
	}

	/**
	 * Set the document to process.
	 */
	public void setDocument(gate.Document document) {
		this.document = document;
	}

	/**
	 * Return the document being processed.
	 */
	public gate.Document getDocument() {
		return document;
	}

	/**
	 * Set the name of the annotation set to place the generated Token
	 * annotations in.
	 */
	public void setAnnotationSetName(String annotationSetName) {
		this.annotationSetName = annotationSetName;
	}

	/**
	 * Return the annotation set name used for the Tokens.
	 */
	public String getAnnotationSetName() {
		return annotationSetName;
	}

	public void setResourceDir(java.net.URL resourceDir) {
		this.resourceDir = resourceDir;
	}

	public java.net.URL getResourceDir() {
		return this.resourceDir;
	}

	public void setLanguageFeatureName(String languageFeatureName) {
		this.languageFeatureName = languageFeatureName;
	}

	public String getLanguageFeatureName() {
		return this.languageFeatureName;
	}

	public void setCoarseCategoryFeatureName(String coarseCategoryFeatureName) {
		this.coarseCategoryFeatureName = coarseCategoryFeatureName;
	}

	public String getCoarseCategoryFeatureName() {
		return this.coarseCategoryFeatureName;
	}

	private String encoding;
	private java.net.URL resourceDir;
	private String annotationSetName;
	private gate.Document document;
	private String languageFeatureName;
	private String coarseCategoryFeatureName;

} // class PosTagMapper

enum COARSE_POS_TAG_SET {
	NOUN, VERB, ADJECTIVE, ADVERB, DETERMINER_OR_PRONOUN
}