{# Used by the action dialog_delete_media #}
<p>Are you sure you want to delete the page “{{ m.media[id].title }}”?</p>

<p>This can't be undone. Your media will be lost forever.</p>

{% button text="Delete" action={delete_media id=id on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
