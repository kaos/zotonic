// widgetManager class.
;(function($)
{
	$.extend(
	{
		widgetManager: function(context)
		{
			context = context || document.body;
			
			var stack 	  = [context];
			
			while(stack.length > 0)
			{
				var objectClass, objectOptions, functionName, defaults, element = stack.pop();
				
				if(element.className && (objectClass = new RegExp("do_([\\w_]+)", "g").exec(element.className)))
				{
					functionName = objectClass[1];
					
					if(typeof $(element)[functionName] == "function")
					{
						if($.ui && typeof $.ui[functionName] == "function")
						{
							defaults = $.ui[functionName].defaults;
						}
						else
						{
							defaults = {}
						}
						
						objectOptions = $.extend({}, defaults, $(element).metadata());							
						$(element)[functionName](objectOptions);
					}
				}

				if(element.childNodes) 
				{
					for(var i in element.childNodes)
					{
						if(element.childNodes[i].nodeType != 3)
						{
							stack.unshift(element.childNodes[i]);
						}
					}
				}
			}
		},
		
		misc: 
		{
			log: function(obj)
			{
				if(window.console) 
				{
					console.log(obj);
	
					if($.noticeAdd)
					{
						$.noticeAdd({
							text: 'Logging, check firebug: '+obj, 
							type: 'notice', 
							stay: 0
						});
					}
				} 
				else 
				{
					if($.noticeAdd)
					{
						$.noticeAdd({
							text: 'logged: '+obj, 
							type: 'notice', 
							stay: 0
						});
					}
					else
					{
						alert(obj.toSource());
					}
				}
			},
			
			warn: function(obj, text)
			{
				if(window.console) 
				{
					console.warn(obj, text);
				}
				
				if($.noticeAdd)
				{
					$.noticeAdd({
						text: text, 
						type: 'notice', 
						stay: 1
					});
				}
			},
			
			error: function(obj, text)
			{
				obj = obj || '';

				if(window.console) 
				{
					console.error(obj, text);
				}
				
				if($.noticeAdd)
				{
					$.noticeAdd({
						text: text, 
						type: 'error', 
						stay: 1
					});
				}
			}
		},
		
		metadata: 
		{
			defaults: {
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
				
				if(data.indexOf( '{' ) < 0)
				data = "{" + data + "}";
				
				data = eval("(" + data + ")");
				
				$.data(elem, settings.single, data);
				return data;
			}
		}
	});
	
	$.fn.metadata = function(opts)
	{
		return $.metadata.get( this[0], opts );
	}
})(jQuery);