$(function()
{
/*	$(window).bind('resize load', function()
	{
		if($(window).width() < 1200)
		{
			$('body').addClass('zp-normal').removeClass('zp-wide');
		}
		else
		{
			$('body').removeClass('zp-normal').addClass('zp-wide');	
		}
	});*/
});	

tinyInit = {
	mode: "exact",
	elements: "field-content",
	theme: "advanced",
	skin: "wp_theme", 
	theme_advanced_buttons1: "bold,italic,strikethrough,|,justifyleft,justifycenter,justifyright,|,bullist,numlist,|,outdent,indent,blockquote,|,undo,redo,|,link,unlink,|,formatselect",
	theme_advanced_buttons2: "",
	theme_advanced_buttons3: "",
	theme_advanced_buttons4: "",
	theme_advanced_toolbar_location: "top", 
	theme_advanced_toolbar_align: "left", 
	theme_advanced_statusbar_location: "bottom", 
	theme_advanced_resizing: "1", 
	theme_advanced_resize_horizontal: "", 
	dialog_type: "modal", 
	relative_urls: "", 
	remove_script_host: "", 
	convert_urls: "", 
	apply_source_formatting: "", 
	remove_linebreaks: "1", 
	paste_convert_middot_lists: "1", 
	paste_remove_spans: "1", 
	paste_remove_styles: "1", 
	gecko_spellcheck: "1", 
	entities: "38,amp,60,lt,62,gt", 
	accessibility_focus: "1", 
	tab_focus: ":prev,:next", 
	content_css: "/lib/js/modules/tinymce/zotonic.css", 
	wpeditimage_disable_captions: "", 
	plugins: "safari"
}
