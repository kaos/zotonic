{# Called by the action_unlink after removing an edge. Enables an undo of the action #}
<div class="notification notice" id="{{ #notice }}">
	{% button text="undo" class="right" 
			action={link subject_id=subject_id predicate=predicate object_id=object_id}
			action={hide} 
			action={slide_fade_out target=#notice} 
		%}
		
	The page “{{m.rsc[object_id].title}}” has been disconnected.
</div>
