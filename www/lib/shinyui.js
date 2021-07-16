shinyjs.isTabdisabled = function(el) {
    var tab = $('.nav li a[data-value=' + el + ']');
    var outstring = 'jscatch_disabled_' + el;
    Shiny.setInputValue(outstring, tab.hasClass('disabled'));
};

shinyjs.isIconhidden = function(el) {
  var icon = $('#' + el);
  var outstring = 'jscatch_icon_' + el;
  Shiny.setInputValue(outstring, icon.css('display') == 'none');
};

shinyjs.disableTab = function(params) {
  var defaultParams = {
    name: null,
    class:'disabled'
  };
  params = shinyjs.getParams(params, defaultParams);
  
  var tab = $('.nav li a[data-value=' + params.name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass(params.class);
};

shinyjs.enableTab = function(params) {
  var defaultParams = {
    name: null,
    class:'disabled'
  };
  params = shinyjs.getParams(params, defaultParams);
  
  var tab = $('.nav li a[data-value=' + params.name + ']');
  tab.unbind('click.tab');
  tab.removeClass(params.class);
};

shinyjs.disableBtn = function(selector, onoff) {
  $(selector).prop('disabled', onoff);
};

shinyjs.toggleTabInputs = function(params){
  var defaultParams = {
    tab_selector: null,
    sub_elements:'*',
    exclude_elements:null,
    disabled:true
  };
  params = shinyjs.getParams(params, defaultParams);

  $(params.tab_selector).find(params.sub_elements).not(params.exclude_elements).prop('disabled', params.disabled);
};