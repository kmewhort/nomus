$.widget( "custom.catcomplete", $.ui.autocomplete, {
		_renderMenu: function( ul, items ) {
			var self = this,
				currentCategory = "";
			$.each( items, function( index, item ) {
				if ( item.category != currentCategory ) {
					ul.append( "<li class='ui-autocomplete-category'>" + item.category + "</li>" );
					currentCategory = item.category;
				}
		  		else{
					self._renderItem( ul, item );
				}
			});
		},
		_renderItem: function( ul, item) {		  
		  if(item.url != null){
		  	return $( '<li class="autocomplete-link"></li>' )
        	  .data( "item.autocomplete", item )
        	  .append( "<a>" +  item.label + "</a>" )
        	   .appendTo( ul );
		  }
		  else if(item.query != null){
		  	 return $( '<li class="autocomplete-more"></li>' )
        	  .data( "item.autocomplete", item )
        	  .append( "<a>" +  item.label + "</a>" )
        	   .appendTo( ul );
		  }
		  else{
          return $( "<li></li>" )
        	  .data( "item.autocomplete", item )
        	  .append( "<a>" +  item.label + "</a>" )
        	   .appendTo( ul );
        }
	   },
	_create: function() {
		var self = this,
			doc = this.element[ 0 ].ownerDocument;
		this.element
			.addClass( "ui-autocomplete-input" )
			.attr( "autocomplete", "off" )
			// TODO verify these actually work as intended
			.attr({
				role: "textbox",
				"aria-autocomplete": "list",
				"aria-haspopup": "true"
			})
			.bind( "keydown.autocomplete", function( event ) {
				if ( self.options.disabled ) {
					return;
				}

				var keyCode = $.ui.keyCode;
				switch( event.keyCode ) {
				case keyCode.PAGE_UP:
					self._move( "previousPage", event );
					break;
				case keyCode.PAGE_DOWN:
					self._move( "nextPage", event );
					break;
				case keyCode.UP:
					self._move( "previous", event );
					// prevent moving cursor to beginning of text field in some browsers
					event.preventDefault();
					break;
				case keyCode.DOWN:
					self._move( "next", event );
					// prevent moving cursor to end of text field in some browsers
					event.preventDefault();
					break;
				case keyCode.ENTER:
				case keyCode.NUMPAD_ENTER:
					// when menu is open or has focus
					if ( self.menu.element.is( ":visible" ) ) {
						event.preventDefault();
					}
					//passthrough - ENTER and TAB both select the current element
				case keyCode.TAB:
					if ( !self.menu.active ) {
						return;
					}
					self.menu.select( event );
					break;
				case keyCode.ESCAPE:
					self.element.val( self.term );
					self.close( event );
					break;
				default:
					// keypress is triggered before the input value is changed
					clearTimeout( self.searching );
					self.searching = setTimeout(function() {
						// only search if the value has changed
						if ( self.term != self.element.val() ) {
							self.selectedItem = null;
							self.search( null, event );
						}
					}, self.options.delay );
					break;
				}
			})
			.bind( "focus.autocomplete", function() {
				if ( self.options.disabled ) {
					return;
				}

				self.selectedItem = null;
				self.previous = self.element.val();
			})
			.bind( "blur.autocomplete", function( event ) {
				if ( self.options.disabled ) {
					return;
				}

				clearTimeout( self.searching );
				// clicks on the menu (or a button to trigger a search) will cause a blur event
				self.closing = setTimeout(function() {
					self.close( event );
					self._change( event );
				}, 150 );
			});
		this._initSource();
		this.response = function() {
			return self._response.apply( self, arguments );
		};
		this.menu = $( "<ul></ul>" )
			.addClass( "ui-autocomplete" )
			.appendTo( $( this.options.appendTo || "body", doc )[0] )
			// prevent the close-on-blur in case of a "slow" click on the menu (long mousedown)
			.mousedown(function( event ) {
				// clicking on the scrollbar causes focus to shift to the body
				// but we can't detect a mouseup or a click immediately afterward
				// so we have to track the next mousedown and close the menu if
				// the user clicks somewhere outside of the autocomplete
				var menuElement = self.menu.element[ 0 ];
				if ( event.target === menuElement ) {
					setTimeout(function() {
						$( document ).one( 'mousedown', function( event ) {
							if ( event.target !== self.element[ 0 ] &&
								event.target !== menuElement &&
								!$.ui.contains( menuElement, event.target ) ) {
								self.close();
							}
						});
					}, 1 );
				}

				// use another timeout to make sure the blur-event-handler on the input was already triggered
				setTimeout(function() {
					clearTimeout( self.closing );
				}, 13);
			})
			.menu({
				focus: function( event, ui ) {
					var item = ui.item.data( "item.autocomplete" );
					if ( false !== self._trigger( "focus", null, { item: item } ) ) {
						// use value to match what will end up in the input, if it was a key event
						if ( /^key/.test(event.originalEvent.type) ) {
							self.element.val( item.value );
						}
					}
				},
				selected: function( event, ui ) {
					var item = ui.item.data( "item.autocomplete" ),
						previous = self.previous;

					// only trigger when focus was lost (click on menu)
					if ( self.element[0] !== doc.activeElement ) {
						self.element.focus();
						self.previous = previous;
					}

					if ( false !== self._trigger( "select", event, { item: item } ) ) {
						self.term = item.value;
						// if the item has an url, open it
						if(item.url != null){
							location.href = Drupal.settings.basePath + item.url;
						} else if(item.query != null){
						   var searchUrl = Drupal.settings.nomus_search.direct_search_url;
		    				searchUrl = searchUrl.replace('#QUERY#',item.query);
		    				searchUrl = searchUrl.replace('#PF#',item.pf);
		    				location.href = searchUrl;
		    			} else{
							self.element.val( item.value );
						}
					}

					self.close( event );
					self.selectedItem = item;
				},
				blur: function( event, ui ) {
					// don't set the value of the text field if it's already correct
					// this prevents moving the cursor unnecessarily
					if ( self.menu.element.is(":visible") &&
						( self.element.val() !== self.term ) ) {
						self.element.val( self.term );
					}
				}
			})
			.zIndex( this.element.zIndex() + 1 )
			// workaround for jQuery bug #5781 http://dev.jquery.com/ticket/5781
			.css({ top: 0, left: 0 })
			.hide()
			.data( "menu" );
		if ( $.fn.bgiframe ) {
			 this.menu.element.bgiframe();
		}
	},

	});
	
$(document).ready(function(){
	$( "#edit-search-block-form-1" ).catcomplete({
		delay: 300,
      minLength: 3,
      source: [],
      search: function(event, ui) {
      	var searchString = this.value;
      	
      	// for each search block
      	$('.block-nomus').each(function(){
      		// display the block
      		$(this).css('display', 'inline-block');
      		
      		// get the search parameters for this block
      		var blockId = $(this).attr('id');
      		blockId = blockId.replace('block-nomus-', '');
      		var searchSettings = Drupal.settings['nomus_search_block_' + blockId];
      		if(typeof(searchSettings) == 'undefined')
      			return;
      		
      		// display throbber	
      		$(this).addClass('block-pending');
      		
      		// abort any outstanding request
      		if(typeof(searchSettings.pendingRequest) != "undefined"){
      			searchSettings.pendingRequest.abort();
      		}
      		
      		// perform the search
      		var searchBlock = this;
      		searchSettings.params.query = searchString;
      		var req = $.getJSON(searchSettings.url,
      			searchSettings.params,
      			function(data){
      				// add each element to the block
      				$(searchBlock).find(".content ul").replaceWith('<ul></ul>');
      				var list = $(searchBlock).find(".content ul");
      				if(typeof(data.results) != "undefined" && data.results.length > 0){
      					for(var i = 0; i < data.results.length; i++){
      						list.append('<li><a href="' + Drupal.settings.basePath + data.results[i].url + '">' + data.results[i].label + '</a></li>');
      					}
      					var allResultsUrl = Drupal.settings.basePath +
      						'search/apachesolr_search/' + searchString + '?template=' + 
      						Drupal.settings['nomus_search_block_' + blockId].template;
      					list.append('<li class="all-link"><a href="' + allResultsUrl + '">See all ' + data.numResults + '...</a></li>');
      				}
      				$(searchBlock).removeClass('block-pending');
      			});
   			Drupal.settings['nomus_search_block_' + blockId].pendingRequest = req;
      	});
      	// don't allow the suggest box to open
      	return false;
      }

	});
});

// new / edit links
Drupal.nomus_blocks_linkopen = function()
{
	// open the dialog
	Drupal.modalFrame.open(
	{
		url: $(this).attr('href'),
		autofit: false,
		width: 600, height: 400,
		onSubmit: function() { window.location.reload(); }
	});
	return false;
};

// register modal links
$(document).ready(function()
{
	// register existing open-edit-dialog with edit buttons
	$('.modal-link').click(
		Drupal.nomus_blocks_linkopen);
});