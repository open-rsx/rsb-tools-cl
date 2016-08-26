// Copyright (C) 2014, 2015, 2016 Jan Moringen
//
// Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

function BreadcrumbsWidget(api, element, args) {
    var self = this;

    this.api     = api;
    this.element = element;
    this.level   = (args && args.level);

    this.element.addClass('breadcrumb');

    function addItem(id, href, html, isActive) {
        self.element.append(' ',
                            $('<li class="' + (isActive ? 'active' : '') + '">'
                              + (!isActive ? '<a id="' + id + '" href="' + href + '">' : '')
                              + html
                              + (!isActive ? '</a>' : '')
                              + '</li>'));
    }

    function addComboItem(id, listId, label, isActive) {
        var labelId = (isActive ? id : 'select' + label);

        var container = $('<li' + (isActive ? ' class="active"' : '') + '></li>');
        self.element.append(' ', container);
        var group = $('<div '
                      + (isActive
                         ? 'class="dropdown" style="float: right"'
                         : 'class="btn-group"')
                     + '></div>');
        container.append(group);

        function makeLabel() {
            return ('<span id="label">'
                    + label
                    + ' <span class="glyphicon glyphicon-refresh glyphicon-spin"></span>'
                    + '</span>');
        }
        if (!isActive) {
            group.append('<a id="' + id + '" class="btn btn-default btn-xs">'
                         + makeLabel()
                         + '</a>');
        }
        group.append('<button id="' + labelId + '"'
                     + '      type="button" class="btn btn-default btn-xs dropdown-toggle"'
                     + '      data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">'
                     + (isActive ? makeLabel() : '')
                     + ' ' + '<span class="caret"></span>'
                     + '</button>');
        group.append('<ul id="' + listId + '" class="dropdown-menu" '
                     + 'aria-labelledby="' + labelId + '"></ul>');
    }

    // Determine level
    var levels = [ '', 'introspection', 'host', 'process', 'participant' ];
    var numericLevel = levels.indexOf(this.level);

    // Build items.
    if (numericLevel >= 0) {
        addItem('root', '../index.html', '<span class="glyphicon glyphicon-home"></span>',
                numericLevel == 0);
    }
    if (numericLevel >= 1) {
        addItem('introspection', 'root.html', 'Introspection', numericLevel == 1);
    }
    if (numericLevel >= 2) {
        addComboItem('host', 'hosts', 'Host', numericLevel == 2);
    }
    if (numericLevel >= 3) {
        addComboItem('process', 'processes', 'Process', numericLevel == 3);
    }
    if (numericLevel >= 4) {
        addComboItem('participant', 'participants', 'Participant', numericLevel == 4);
    }
}

BreadcrumbsWidget.prototype.refresh = function(info) {
    function updateOneLevel(selectedElement, listElement,
                            selected, all,
                            URLFunction, descriptionFunction) {
        selectedElement
            .attr('href', URLFunction(selected, info.level)) // does not always make sense
            .children('#label').html(descriptionFunction(selected));

        listElement.find('li').remove();
        $.each(all, function(k, item) {
            listElement.append('<li><a href=\''
                               + URLFunction(item, info.level)
                               + '\'>'
                               + descriptionFunction(item)
                               + '</a></li>');
        });
    }

    var host        = info['host'];
    var process     = info['process'];
    var participant = info['participant'];
    if (host) {
        updateOneLevel(this.element.find('#host'),
                       this.element.find('#hosts'),
                       host, info['hosts'],
                       hostURL, hostDescription);
    }
    if (process) {
        updateOneLevel(this.element.find('#process'),
                       this.element.find('#processes'),
                       process, info['processes'],
                       function (process, level) {
                           return processURL(host, process, level);
                       },
                       processDescription);
    }
    if (participant) {
        updateOneLevel(this.element.find('#participant'),
                       this.element.find('#participants'),
                       participant, info['participants'],
                       function (participant, level) {
                           return participantURL(host, process, participant, level);
                       },
                       participantDescription);
    }
};
