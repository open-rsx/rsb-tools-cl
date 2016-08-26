// Copyright (C) 2014, 2015, 2016 Jan Moringen
//
// Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

// requires moment.js

/// Utility functions

function makeTitle(kind, id, title) {
    var urlSpec = {};
    urlSpec[id] = undefined;
    return $('<td> \
                <a href="' + makeSearchURL(kind, urlSpec) + '">'
             + title
             + '</a> \
              </td>');
}

function makeSimpleValue(id) {
    return $('<td id="' + id + '-value" style="font-family: monospace"> \
                <span class="glyphicon glyphicon-refresh glyphicon-spin"></span> \
              </td>');
}

function updateSimpleValue(element, kind, id, value) {
    var urlSpec = {};
    urlSpec[id] = value;
    var url     = makeSearchURL(kind, urlSpec);
    element.find('#' + id + '-value').html('<a href="' + url + '">' + value + '</a>');
}

function makeTimeValue(id) {
    return $('<td> \
                <span id="' + id + '-value" style="font-family: monospace"> \
                  <span class="glyphicon glyphicon-refresh glyphicon-spin"></span> \
                </span> \
                <span id="' + id + '-livestamp" style="font-style: italic"> \
                  <span class="glyphicon glyphicon-refresh glyphicon-spin"></span> \
                </span> \
              </td>');
}

function updateTimeValue(element, kind, id, value) {
    var time = moment(value);
    element.find('#' + id + '-value').html(time.format());
    element.find('#' + id + '-livestamp').html('(' + time.fromNow() + ')');
}

/// Details widget

function DetailsWidget(element, specs) {
    var self = this;

    this.specs = specs;

    this.element = element;
    this.element.addClass('table');

    $.each(this.specs.lines,
           function(i, spec) {
               var row = $('<tr/>');
               row.append(makeTitle(specs.kind, spec.id, spec.title));
               row.append(spec.valueMaker(spec.id));
               self.element.append(row);
           });
}

DetailsWidget.prototype.refresh = function updateDetails(object) {
    var self = this;

    $.each(this.specs.lines,
           function (i, spec) {
               spec.valueUpdater(self.element,
                                 self.specs.kind, spec.id, object[spec.id]);
           });
}

/// Participant

var participantDetailSpecs = {
    kind:  'participant',
    lines: [
        {
            title:        'Scope',
            id:           'scope',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Kind',
            id:           'kind',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'State',
            id:           'state',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Type',
            id:           'type',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'ID',
            id:           'id',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Transports',
            id:           'transports',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        }
    ]};

/// Process

var processDetailSpecs = {
    kind:  'process',
    lines: [
        {
            title:        'Process ID',
            id:           'process-id',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Executing User',
            id:           'executing-user',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Start Time',
            id:           'start-time',
            valueMaker:   makeTimeValue,
            valueUpdater: updateTimeValue
        },
        {
            title:        'Program Name',
            id:           'program-name',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Commandline Arguments',
            id:           'commandline-arguments',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'State',
            id:           'state',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'RSB Version',
            id:           'rsb-version',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Transports',
            id:           'transports',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        }
    ]
};

/// Host

var hostDetailSpecs = {
    kind:  'host',
    lines: [
        {
            title:        'ID',
            id:           'id',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Hostname',
            id:           'hostname',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Machine Type',
            id:           'machine-type',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Machine Version',
            id:           'machine-version',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Software Type',
            id:           'software-type',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        },
        {
            title:        'Software Version',
            id:           'software-version',
            valueMaker:   makeSimpleValue,
            valueUpdater: updateSimpleValue
        }
    ]
};
