package org.knallgrau.utils.textcat;

import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import gate.creole.metadata.CreoleParameter;
import gate.creole.metadata.CreoleResource;
import gate.creole.metadata.Optional;
import gate.creole.metadata.RunTime;

import org.knallgrau.utils.textcat.TextCategorizer;

@CreoleResource(name = "TextCat PR", comment = "Recognizes the document language using TextCat. Possible" +
		"languages: german, english, french, spanish, italian, swedish, polish, dutch, norwegian, finnish, albanian" +
		"slovakian, slovenian, danish, hungarian.")		
public class LanguageIdentifier extends gate.creole.AbstractLanguageAnalyser {

	private static final long serialVersionUID = 5831213212185693826L;
	
	public LanguageIdentifier init() throws ResourceInstantiationException {
		return this;
	}

	/**
	 * Based on the document content, recognizes the language and adds a document feature.
	 */
	public void execute() throws ExecutionException {
		if(document == null || document.getFeatures() == null)
			return;
		
		String language = (String) document.getFeatures().get(languageFeatureName);
		if(language != null && language.length() > 0)
			return;
		if(languageFeatureName == null || "".equals(languageFeatureName))
			languageFeatureName = "language";
		TextCategorizer guesser = new TextCategorizer();
	    String category = guesser.categorize(document.getContent().toString());
	    document.getFeatures().put(languageFeatureName, category);
	}

	public void reInit() throws ResourceInstantiationException { }

	private String languageFeatureName;
	
	public String getLanguageFeatureName() {
		return languageFeatureName;
	}

	@RunTime
	@Optional
	@CreoleParameter(comment = "Name of the document feature which will be used for language.", defaultValue = "LANGUAGE")
	public void setLanguageFeatureName(String languageFeatureName) {
		this.languageFeatureName = languageFeatureName;
	}
} // class LanguageIdentifier

