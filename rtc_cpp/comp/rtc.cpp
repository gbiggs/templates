/* RTC:
 *
 * Copyright (C) 2011
 *     Geoffrey Biggs
 *     RT-Synthesis Research Group
 *     Intelligent Systems Research Institute,
 *     National Institute of Advanced Industrial Science and Technology (AIST),
 *     Japan
 *     All rights reserved.
 * Licensed under the Eclipse Public License -v 1.0 (EPL)
 * http://www.opensource.org/licenses/eclipse-1.0.txt
 *
 * Component source file.
 */


#include "rtc.h"


RTC::RTC(RTC::Manager* manager)
    : RTC::DataFlowComponentBase(manager),
    _port_("<PORT_NAME>", _),
    //svc_prov_(),
    //svc_port_("<PORT_NAME>")
{
}


RTC::~RTC()
{
}


RTC::ReturnCode_t RTC::onInitialize()
{
    bindParameter("", _, "");
    /*std::string active_set =
        m_properties.getProperty("configuration.active_config", "default");
    m_configsets.update(active_set.c_str());*/

    addInPort(_port_.getName(), _port_);
#if defined(RTDOC_SUPPORT)
    //_port_.addProperty("description", "");
#endif //defined(RTDOC_SUPPORT)
    addOutPort(_port_.getName(), _port_);
#if defined(RTDOC_SUPPORT)
    //_port_.addProperty("description", "");
#endif //defined(RTDOC_SUPPORT)
    //svc_port_.registerProvider("<INSTANCE_NAME>", "<TYPE_NAME>", svc_prov_);
    //addPort(svc_port_);
#if defined(RTDOC_SUPPORT)
    //svc_port_.addProperty("description", "");
#endif //defined(RTDOC_SUPPORT)

    return RTC::RTC_OK;
}


/*RTC::ReturnCode_t RTC::onFinalize()
{
    return RTC::RTC_OK;
}*/


/*RTC::ReturnCode_t RTC::onActivated(RTC::UniqueId ec_id)
{
    return RTC::RTC_OK;
}*/


/*RTC::ReturnCode_t RTC::onDeactivated(RTC::UniqueId ec_id)
{
    return RTC::RTC_OK;
}*/


/*RTC::ReturnCode_t RTC::onExecute(RTC::UniqueId ec_id)
{

    return RTC::RTC_OK;
}*/


static const char* spec[] =
{
    "implementation_id", "<COMP>",
    "type_name",         "<COMP_TYPE>",
    "description",       "",
    "version",           "1.0",
    "vendor",            "Geoffrey Biggs, AIST",
    "category",          "",
    "activity_type",     "PERIODIC",
    "kind",              "DataFlowComponent",
    "max_instance",      "1",
    "language",          "C++",
    "lang_type",         "compile",
    // Configuration variables
    //"conf.default.", "",
#if defined(RTDOC_SUPPORT)
    //"conf.__description__.", "",
#endif //defined(RTDOC_SUPPORT)
    // Widget
    //"conf.__widget__.", "text",
    //"conf.__widget__.", "spin",
    // Constraints
    //"conf.__constraints__.", "0<=x<=100",
    // Documentation
#if defined(RTDOC_SUPPORT)
    //"conf.__doc__.__order__", "",
    //"conf.__doc__.__license__", "",
    //"conf.__doc__.__contact__", "",
    //"conf.__doc__.__url__", "",
    //"conf.__doc__.intro", "",
    //"conf.__doc__.reqs", "",
    //"conf.__doc__.install", "",
    //"conf.__doc__.usage", "",
    //"conf.__doc__.misc", "",
    //"conf.__doc__.changelog", "",
#endif //defined(RTDOC_SUPPORT)
    ""
};

extern "C"
{
    void rtc_init(RTC::Manager* manager)
    {
        coil::Properties profile(spec);
        manager->registerFactory(profile, RTC::Create<RTC>,
                RTC::Delete<RTC>);
    }
};

