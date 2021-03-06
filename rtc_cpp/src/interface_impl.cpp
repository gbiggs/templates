/* RTC<component>
 *
 * <interface> interface implementation.
 *
 * Copyright 2010-2011 Geoffrey Biggs geoffrey.biggs@aist.go.jp
 *     RT-Synthesis Research Group
 *     Intelligent Systems Research Institute,
 *     National Institute of Advanced Industrial Science and Technology (AIST),
 *     Japan
 *     All rights reserved.
 *
 * This file is part of RTC<component>.
 *
 * RTC<component> is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License,
 * or (at your option) any later version.
 *
 * RTC<component> is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with RTC<component>. If not, see <http://www.gnu.org/licenses/>.
 */


#include <rtc<component>/<interface>_impl.h>
#include <rtc<component>/rtc<component>.h>

using namespace <module>;

//////////////////////////////////////////////////////////////////////////////
// <interface> provider class

<interface>Provider::<interface>Provider()
    : owner_(0)
{
}


<interface>Provider::~<interface>Provider()
{
}

