$(document).ready(function()
{
	$('#edit-template-wrapper select').selectmenu(
	{
		style:'dropdown',
		width: 180,
		menuWidth: 180,
		format: Drupal.nomus_searchtemplate_addlinks
	});
	
	// register open-edit-dialog with edit buttons
	$('.searchtemplate-link').click(
		Drupal.nomus_searchtemplate_linkopen);
});

Drupal.nomus_searchtemplate_addlinks = function(text)
{
	// add edit link
	newText = text.replace(/edit(\d+)/,
		'<div class="searchtemplate-editlink"><a href="' + Drupal.settings.basePath + 'node/' +
		"$1" + '/edit" class="searchtemplate-link">edit</a></div>');
		
	// add clone link
	newText = newText.replace(/clone(\d+)/,
		'<div class="searchtemplate-editlink"><a href="' + Drupal.settings.basePath + 'node/' +
		"$1" + '/clone" class="searchtemplate-link">edit</a></div>');
		
	// add delete link
	newText = newText.replace(/delete(\d+)/,
		'<div class="searchtemplate-deletelink"><a href="' + Drupal.settings.basePath + 'node/' +
		"$1" + '/delete" class="searchtemplate-link">delete</a>');
		
	// add New... link
	newText = newText.replace(/New\.\.\./,
		'<a href="' + Drupal.settings.basePath + 'node/add/search-template' +
		'" class="searchtemplate-link searchtemplate-newlink">New...</a>');

	return newText;
}

Drupal.nomus_searchtemplate_linkopen = function()
{
	// open the dialog
	Drupal.modalFrame.open(
	{
		url: $(this).attr('href'),
		width: 600, height: 400,
		onSubmit: function() { window.location.reload(); }
	});
	return false;
};