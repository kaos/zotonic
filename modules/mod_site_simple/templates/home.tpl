{% extends "base.tpl" %}

{% block title %}
	Simple Home
{% endblock %}

{% block content %}

	<div class="zp-70" id="content">
		<div class="padding">
	
			<h1>Atatonic CSS Framework</h1>

			<h2>A CSS Study by Tim Benniks</h2>
			<p class="intro">Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
	
			<h3>What another framework?</h3>
			<p>Lorem ipsum dolor sit amet, <a href="http://www.timbenniks.com">consectetur</a> adipisicing elit, sed do eiusmod tempor ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
				
			<hr />	
				
			<div class="zp-50 item">

				<div class="padding">
					<h4>Testing 1</h4>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
				</div>
			</div>
			<div class="zp-50 item">
				<div class="padding last">
					<h4>Testing 2</h4>

					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
				</div>
			</div>
			
			<hr class="clear" />
			
			<h2>This is even better</h2>
			<p>
				Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
			</p>

			
			<div class="button-wrap clearfix">
				<button>I am a button</button>
				<a class="button" href="#">I am a link with class button</a>
			</div>
			
			<form method="post" action="">
				<fieldset>
					<legend>Form example</legend>
	
					<div class="notification error">The email is not correct</div>
					<div class="notification notice">That looks like a weird city to live in.</div>
	
					<div class="form-item">
						<label for="name">Name:</label>
						<input tabindex="1" id="name" type="text" name="name" />
					</div>
					
					<div class="form-item">

						<label for="street">Street name:</label>
						<input tabindex="1" id="street" type="text" name="street" />
					</div>	
					
					<div class="form-item">
						<label for="city">City:</label>
						<input tabindex="2" id="city" type="text" name="city" class="form-field-notice" value="Mars" />
					</div>
					
					<div class="form-item">

						<label for="email">Email:</label>
						<input tabindex="3" id="email" type="text" name="e-mail" class="form-field-error" value="@atatonic.net" />
					</div>
					
					<div class="form-item clearfix">
						<label for="message">Message:</label>
						<textarea tabindex="4" id="message" name="message" rows="20" cols="20" class="zp-80"></textarea>
					</div>
	
					<div class="form-item">
						<label for="Lorem">Dropper:</label>
						<select tabindex="5" id="Lorem"  name="Lorem ipsum">
							<option selected="selected" value="1">Lorem ipsum</option>
							<option value="2">Dolor sit amet</option>
							<option value="3">Consectetuer adipiscing</option>
							<option value="4">Vitae diam</option>

							<option value="5">Vestibulum ornare</option>
						</select>
					</div>	
					<button>Send form</button>
				</fieldset>
			</form>
		</div>
	</div>

{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-30">
		
		<h1>Sidebar</h1>
		<h2>A title in the sidebar</h2>
		<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>

		<h3>What a nice rhythm</h3>
		<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>

		<hr />

		<div class="list-item">
			<h2>Some other stuff</h2>
			<h3>With a sweet subtitle with weird margins</h3>
			<p>
				<img src="http://static.timbenniks.nl/timbenniks/image/helvetica-80-80.jpg" alt="Helvetica" />
				Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
			</p>

		</div>

		<div class="notification notice">
			<h5>Atatonic notice</h5>
			This is a basic notice message, without a header it might even look nicer. 
		</div>

		<div class="notification success">
			<h5>Success</h5>
			This is a basic success message
		</div>


		<div class="notification error">
			<h5>Error</h5>
			Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
		</div>
		
	</div>
{% endblock %}
