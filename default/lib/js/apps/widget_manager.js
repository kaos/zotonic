/*
 * widgetManager 1.0
 *
 * Copyright (c) 2009 Tim Benniks <tim@timbenniks.com>
 * Licensed under the MIT
 *
 * http://www.zophrenic.com
 *
 */

// create a basic namespace for all widgets in Zophrenic
$.zp = $.zp || {}; 

// widgetManager class.
;(function($)
{
	$.extend(
	{
		widgetManager: 
		{
			init: function(widgets, context)
			{
				context = context || document.body;
				
				this.widgetNames = widgets;
				this.startWidgets(context);
			},
			
			startWidgets: function(context)
			{
				var stack 	 = [context];
				var names	 = this.widgetNames;
				var names_do = {};
				
				for(var i in names)
			   	{
					names_do['do_' + names[i]] = names[i];
			   	}

				while(stack.length > 0)
				{
					var element = stack.pop();
					
					if(element.className && element.className.match( /do_[a-z_]+/ ))
					{
						// Note: IE6 does not recognize the \b escape char (whitespace in js regexps)
						var objectClass = element.className.match( /do_[a-z_]+/g );
					
						for(var i in objectClass)
						{
							var functionName = names_do[objectClass[i]];
							
							if(functionName)
							{
								// use the metadata plugin to add options
								eval('$(element).' + functionName + '()');
							}
						}
					}
					
					if(element.childNodes) 
					{
						for(var i in element.childNodes)
						{
							if(element.childNodes[i].nodeType != 3)
							{
								// Only process if not a text node
								stack.unshift(element.childNodes[i]);
							}
						}
					}
				}
			}		
		}
	});
})(jQuery);

// metadata plugin
;(function($) 
{
	$.extend(
	{
		metadata: 
		{
			defaults : {
				type: 'class',
				name: 'metadata',
				cre: /({.*})/,
				single: 'metadata'
			},
			
			setType: function(type, name)
			{
				this.defaults.type = type;
				this.defaults.name = name;
			},
			
			get: function(elem, opts)
			{
				var settings = $.extend({}, this.defaults, opts);
				
				// check for empty string in single property
				if(!settings.single.length)
				{
					settings.single = 'metadata';
				}
				
				var data = $.data(elem, settings.single);
				
				// returned cached data if it already exists
				if(data)
				{
					return data;
				}
				
				data = "{}";
				
				if(settings.type == "class")
				{
					var m = settings.cre.exec(elem.className);
					if(m)
					{
						data = m[1];
					}
				} 
				else if(settings.type == "elem")
				{
					if(!elem.getElementsByTagName)
					{
						return;
					}
					
					var e = elem.getElementsByTagName(settings.name);
					
					if(e.length)
					{
						data = $.trim(e[0].innerHTML);
					}
				} 
				else if(elem.getAttribute != undefined)
				{
					var attr = elem.getAttribute(settings.name);
					if(attr)
					{
					data = attr;
					}
				}
				
				if ( data.indexOf( '{' ) < 0 )
				data = "{" + data + "}";
				
				data = eval("(" + data + ")");
				
				$.data( elem, settings.single, data );
				return data;
			}
		}
	});
	
	$.fn.metadata = function(opts)
	{
		return $.metadata.get( this[0], opts );
	}
})(jQuery);