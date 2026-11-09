(function() {
  function fixTooltipTarget() {
    if (typeof shinyBS === 'undefined' ||
        typeof shinyBS.getTooltipTarget !== 'function' ||
        typeof shinyBS.addTooltip !== 'function') {
      return;
    }

    if (!shinyBS.getTooltipTarget.__pmartRFixed) {
      var getTooltipTarget = shinyBS.getTooltipTarget;
      var getTooltipTargetWithFallback = function(id) {
        var target = getTooltipTarget(id);
        return target.length ? target : $('#' + id);
      };

      getTooltipTargetWithFallback.__pmartRFixed = true;
      shinyBS.getTooltipTarget = getTooltipTargetWithFallback;
    }

    if (!shinyBS.addTooltip.__pmartRFixed) {
      var addTooltip = shinyBS.addTooltip;
      var addTooltipWithTitle = function(id, type, opts) {
        var target = shinyBS.getTooltipTarget(id);
        if (target.length && opts && opts.title) {
          target.attr('title', opts.title);
        }
        return addTooltip(id, type, opts);
      };

      addTooltipWithTitle.__pmartRFixed = true;
      shinyBS.addTooltip = addTooltipWithTitle;
    }
  }

  $(document).ready(fixTooltipTarget);
  $(document).on('shiny:connected', fixTooltipTarget);
  fixTooltipTarget();
}());

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