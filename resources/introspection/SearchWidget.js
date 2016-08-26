// Copyright (C) 2014, 2015, 2016 Jan Moringen
//
// Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

// TODO put this somewhere and use it here
function htmlEncode(value) {
    return $('<div/>').text(value).html();
}

/// Basic search widget

function SearchWidget(api, element) {
    var self = this;

    this.element = element;
    this.api     = api;
    this.query   = undefined;

    this.element.html('<div class="input-group"> \
                         <span class="input-group-addon"> \
                           <span class="glyphicon glyphicon-search"></span> \
                         </span> \
                         <input id="query" name="query" type="text" class="form-control"  \
                                placeholder="Host, Process, Participant substring"> \
                         </input> \
                         <input id="start" name="start" type="hidden" value="0"></input> \
                         <input id="limit" name="limit" type="hidden" value="20"></input> \
                       </div>');
    //this.query = this.element.children('#query'); TODO
    this.element.on('submit', function() {
        self.api.search($(this).serialize())
            .done(function (result) {
                self.element.trigger('result', [ result ]);
            })
            .fail(function (message) {
                self.element.trigger('error', [ message ]);
            });
        return false;
    });
}

SearchWidget.prototype.search = function() {
    this.element.trigger('submit');
};

SearchWidget.prototype.getValue = function () {
    return this.element.find('#query')[0].value;
};

SearchWidget.prototype.setValue = function (newValue) {
    this.element.find('#query')[0].value = newValue;
};

SearchWidget.prototype.on = function (event, handler) {
    return this.element.on(event, handler);
};

/// Global search widget

function GlobalSearchWidget(api, element) {
    var search = new SearchWidget(api, element);
    search.on('submit', function(event) {
        window.location = 'search.html?query=' + search.getValue();
    });
    return search;
}

/// Utility functions

function showRunning(listElement, countElement) {
    if (countElement) {
        countElement.html('');
    }
    if (listElement) {
        listElement.children().remove();
        listElement.append('<div>Searching&nbsp;<span class="glyphicon glyphicon-refresh glyphicon-spin"></span></div>');
    }
}

function showResults(listElement, countElement, plotElement, results, options) {
    var max   = options && options['max'];
    var flat  = options && options['flat'];
    var count = results.length;
    if (max) {
        results = results.slice(0, max);
    }

    function isComposite(thing) {
        return (!(typeof(thing) === 'string' || typeof(thing) === 'number'));
    }

    function renderFragment(container, result) {
        if (!flat && result.parent) {
            renderFragment(container, result.parent);
            container.append('&nbsp;»&nbsp');
        }

        var item;
        if (result.__kind === 'host') {
            item = $('<a/>')
                   .attr('href', hostURL(result))
                   .html(hostDescription(result));
        } else if (result.__kind === 'process') {
            var host = result.parent;
            item = $('<a/>')
                   .attr('href', processURL(host, result))
                   .html(processDescription(result));
        } else if (result.__kind === 'participant') {
            var process = result;
            while (process.__kind !== 'process') {
                process = process.parent;
            }
            var host    = process.parent;
            item = $('<a/>')
                   .attr('href', participantURL(host, process, result))
                   .html(participantDescription(result));
        } else if (typeof result.__kind !== 'undefined') {
            item = $('<div>' + result.__kind + '</div>');
        }
        container.append(item);
        return !(typeof item === 'undefined');
    }

    var counts = {};
    function countValue(name, value) {
        var values = (counts[name] || (counts[name] = {}));
        values[value] = (values[value] || 0) + 1;
    }

    function renderValue(value) {
        if (value && typeof value === 'object') {
            var plotElement = $('<div style="width: 400px; height: 100px;"></div>');
            $.plot(plotElement,
                   [ $.map(value, function (value, index) { return [ [ index, value ] ]; }) ]);
            return plotElement;
        } else if (!isComposite(value)) {
            return $('<code>' + htmlEncode(value) + '</code>'); // TODO do something else
        } else {
            return $('<code>' + htmlEncode(value) + '</code>');
        }
    }

    function renderResult(element, result) {
        var item =$('<span class="list-group-item"></span>');
        if (result.element) {
            if (!renderFragment(item, result.element)) {
                item.append(renderValue(result.element));
            }
        }
        var name  = result['attribute-name'];
        var value = result['attribute-value'];
        if (name && (typeof value !== 'undefined')) {
            var attributeElement = $('<div>In attribute '
                                     + '<code>' + name + '</code>'
                                     + ', value '
                                     + '</div>');
            attributeElement.append(renderValue(value));
            item.append(attributeElement);

            if (!isComposite(value)) {
                countValue(name, value);
            }
        }
        element.append(item);
    }

    // Rendering.
    if (countElement) {
        countElement.html((isComposite(results) && count > 0) ? count : '');
    }
    if (listElement) {
        listElement.children().remove(); // removes spinner
        if (isComposite(results)) {
            if (results.length > 0) {
                var element = $('<span class="list-group"></span>');
                listElement.append(element);
                $.map(results, function (result) { renderResult(element, result); });
            }
            else {
                listElement.append('<div class="alert alert-info" role="alert">No matches</div>');
            }
        } else {
            listElement.append(renderValue(results));
        }
    }

    if (plotElement) {
        plotElement.children().remove();
        if (counts !== {}) {
            plotElement.show(200);
            $.map(counts, function(values, name) {
                if (values && (Object.keys(values).length > 1)) {
                    plotElement.append('<div>\
                                          In attribute <code>' + name + '</code>:\
                                        </div>');
                    var plot = $('<div style="width: 200px; height: 200px;"></div>');
                    plotElement.append(plot);
                    $.plot(plot,
                           $.map(values, function(count, value) {
                               return { label: value, data: count };
                           }),
                           { series: { pie: { show: true } } });
                }
            });
        } else {
            plotElement.hide(200);
        }
    }
}

function showError(listElement, countElement, error) {
    if (countElement) {
        countElement.html('');
    }
    if (listElement) {
        listElement.children().remove(); // removes spinner
        listElement.append('<pre class="alert alert-danger" role="alert">'
                           + error
                           + '</pre>');
    }
}
